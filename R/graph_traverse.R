# R/graph_traverse.R
# Traversal and relevance-scoring helpers for rrlm_graph objects.
# Covers rrlmgraph issue #10 (Sprint 2).

# ---- compute_relevance -----------------------------------------------

#' Compute composite relevance score for a graph node
#'
#' Calculate a composite relevance score for \code{node} given a query
#' embedding and the traversal state.  The score is a weighted linear
#' combination of four signals:
#'
#' \deqn{
#'   \text{relevance} = 0.40 \cdot \text{sem\_sim}
#'                    + 0.25 \cdot \text{pagerank}
#'                    + 0.25 \cdot \text{task\_trace\_weight}
#'                    + 0.10 \cdot \text{cochange\_score}
#' }
#'
#' The weights can be overridden globally via
#' \code{options(rrlmgraph.weights = list(semantic=, pagerank=,
#' task_trace=, cochange=))}.
#'
#' @section Signal definitions:
#' \describe{
#'   \item{sem_sim}{Cosine similarity between the node's TF-IDF (or other)
#'     embedding and \code{query_vec}.  Clamped to \eqn{[0, 1]}.}
#'   \item{pagerank}{Pre-computed \code{pagerank} vertex attribute,
#'     min-max normalised across the full graph to \eqn{[0, 1]}.}
#'   \item{task_trace_weight}{Vertex attribute set by
#'     \code{update_task_weights()} (issue \#13).  Defaults to \eqn{0.5}
#'     (neutral) when the attribute is absent or \code{NA}.}
#'   \item{cochange_score}{Mean weight of \code{CO_CHANGES} edges
#'     connecting this node to already-visited nodes.  Zero when
#'     \code{visited} is empty or no such edges exist.}
#' }
#'
#' @param node Character(1).  Name of the vertex (i.e., \code{node_id})
#'   to score.
#' @param query_vec Numeric vector.  Dense embedding of the user query
#'   (from \code{embed_query()}).  Pass \code{numeric(0)} to disable
#'   the semantic signal.
#' @param visited Character vector.  Node names already visited in the
#'   current traversal session.  Used to compute the co-change signal.
#'   Defaults to \code{character(0)}.
#' @param graph An \code{rrlm_graph} / \code{igraph} object.
#' @param weights Named list or \code{NULL}.  If non-\code{NULL},
#'   overrides the corresponding default weight(s).  Recognised names:
#'   \code{semantic}, \code{pagerank}, \code{task_trace}, \code{cochange}.
#'   Falls back to \code{getOption("rrlmgraph.weights")}.
#'
#' @return Numeric(1) in \eqn{[0, 1]}.
#'
#' @seealso [build_rrlm_graph()], [embed_query()], [cosine_similarity()]
#' @export
#' @examples
#' \dontrun{
#' g   <- build_rrlm_graph("mypkg")
#' emb <- embed_nodes(extract_function_nodes(detect_rproject("mypkg")$r_files))
#' q   <- embed_query("load training data", emb$model)
#' score <- compute_relevance("data::load_data", q, graph = g)
#' }
compute_relevance <- function(
  node,
  query_vec,
  visited = character(0),
  graph,
  weights = getOption("rrlmgraph.weights")
) {
  w <- .relevance_weights(weights)

  # Locate vertex ---------------------------------------------------------
  v_names <- igraph::V(graph)$name
  v_idx <- match(node, v_names)
  if (is.na(v_idx)) {
    cli::cli_warn("compute_relevance: node {.val {node}} not found in graph.")
    return(0)
  }

  # 1. Semantic similarity ------------------------------------------------
  emb_list <- igraph::vertex_attr(graph, "embedding", index = v_idx)
  emb <- if (is.list(emb_list)) emb_list[[1L]] else emb_list
  sem_sim <- if (
    !is.null(emb) &&
      length(emb) > 0L &&
      !is.null(query_vec) &&
      length(query_vec) > 0L
  ) {
    tryCatch(cosine_similarity(emb, query_vec), error = function(e) 0)
  } else {
    0
  }
  sem_sim <- max(0, min(1, sem_sim))

  # 2. PageRank (min-max normalised) --------------------------------------
  all_pr <- as.numeric(igraph::V(graph)$pagerank)
  pr_norm <- if (!is.null(all_pr) && !all(is.na(all_pr))) {
    pr_raw <- all_pr[[v_idx]]
    pr_max <- max(all_pr, na.rm = TRUE)
    pr_min <- min(all_pr, na.rm = TRUE)
    if (pr_max > pr_min) (pr_raw - pr_min) / (pr_max - pr_min) else 0.5
  } else {
    0.5
  }
  pr_norm <- max(0, min(1, if (is.na(pr_norm)) 0.5 else pr_norm))

  # 3. Task-trace weight (vertex attribute, default 0.5) ------------------
  ttw_all <- igraph::V(graph)$task_trace_weight
  ttw <- if (!is.null(ttw_all) && length(ttw_all) >= v_idx) {
    ttw_all[[v_idx]]
  } else {
    NA_real_
  }
  if (is.null(ttw) || length(ttw) == 0L || is.na(ttw)) {
    ttw <- 0.5
  }
  ttw <- max(0, min(1, ttw))

  # 4. Co-change score ----------------------------------------------------
  cochange <- .cochange_score(graph, v_idx, node, visited)

  # Composite score -------------------------------------------------------
  score <- w$semantic *
    sem_sim +
    w$pagerank * pr_norm +
    w$task_trace * ttw +
    w$cochange * cochange

  max(0, min(1, score))
}

# ---- internal helpers ------------------------------------------------

#' @keywords internal
.relevance_weights <- function(weights) {
  defaults <- list(
    semantic = 0.40,
    pagerank = 0.25,
    task_trace = 0.25,
    cochange = 0.10
  )
  # Merge option-level overrides
  opt_w <- getOption("rrlmgraph.weights")
  if (!is.null(opt_w)) {
    for (nm in names(opt_w)) {
      if (nm %in% names(defaults)) {
        defaults[[nm]] <- as.numeric(opt_w[[nm]])
      }
    }
  }
  # Merge call-level overrides
  if (!is.null(weights)) {
    for (nm in names(weights)) {
      if (nm %in% names(defaults)) {
        defaults[[nm]] <- as.numeric(weights[[nm]])
      }
    }
  }
  defaults
}

#' @keywords internal
.cochange_score <- function(graph, v_idx, v_name, visited) {
  if (length(visited) == 0L) {
    return(0)
  }

  inc <- tryCatch(
    igraph::incident(graph, v_idx, mode = "all"),
    error = function(e) integer(0)
  )
  if (length(inc) == 0L) {
    return(0)
  }

  et <- igraph::E(graph)$edge_type
  co_mask <- seq_along(et) %in%
    as.integer(inc) &
    !is.na(et) &
    et == "CO_CHANGES"
  if (!any(co_mask)) {
    return(0)
  }

  co_ids <- which(co_mask)
  ends_mat <- igraph::ends(graph, co_ids, names = TRUE)
  if (!is.matrix(ends_mat)) {
    ends_mat <- matrix(ends_mat, ncol = 2L)
  }
  nbr_names <- ifelse(ends_mat[, 1L] == v_name, ends_mat[, 2L], ends_mat[, 1L])

  shared_mask <- nbr_names %in% visited
  if (!any(shared_mask)) {
    return(0)
  }

  wts <- igraph::E(graph)$weight[co_ids[shared_mask]]
  sc <- mean(wts, na.rm = TRUE)
  if (is.na(sc)) {
    return(0)
  }
  max(0, min(1, sc))
}

# ---- query_context ---------------------------------------------------

#' Query the rrlm graph and retrieve a token-budgeted context
#'
#' Performs a relevance-guided breadth-first search starting from a
#' \emph{seed node} and builds a context string that fits within
#' \code{budget_tokens}.  The budget is a \strong{hard constraint}: the
#' function never returns more tokens than requested.
#'
#' @section Algorithm:
#' \enumerate{
#'   \item Embed \code{query} with \code{embed_query()}.
#'   \item Identify the seed node: if \code{seed_node} is \code{NULL},
#'         select the function-type vertex with the highest pre-computed
#'         PageRank; otherwise validate and use the supplied name.
#'   \item Initialise \code{visited = \{seed\}} and
#'         \code{frontier = neighbours(seed)}.
#'   \item BFS loop while \code{tokens_used < budget_tokens} and
#'         \code{frontier} is non-empty:
#'         \itemize{
#'           \item Score every frontier node with \code{compute_relevance()}.
#'           \item Select the best-scoring node with score
#'                 \code{>= min_relevance}.
#'           \item Compute its token cost (\code{nchar / 4}).
#'           \item Accept the node only if adding it stays within the
#'                 budget; otherwise skip and try the next-best.
#'           \item Mark as visited; expand its neighbours into the
#'                 frontier.
#'         }
#'   \item Call \code{update_task_weights()} to update the learning trace.
#'   \item Assemble the final context string with
#'         \code{assemble_context_string()}.
#' }
#'
#' @param graph An \code{rrlm_graph} / \code{igraph} object created by
#'   \code{build_rrlm_graph()}.
#' @param query Character(1).  User query string.
#' @param seed_node Character(1) or \code{NULL}.  Name of the vertex to
#'   start traversal from.  \code{NULL} (default) triggers automatic
#'   selection: the function-type node with the highest PageRank.
#' @param budget_tokens Integer(1).  Hard token limit.  Default
#'   \code{2000L}.
#' @param min_relevance Numeric(1).  Minimum relevance score
#'   \eqn{[0, 1]} for a node to be admitted.  Default \code{0.1}.
#' @param max_nodes Integer(1).  Maximum number of nodes (excluding the
#'   seed) to absorb.  Default \code{20L}.
#' @param method Character(1) or \code{NULL}.  Embedding method passed
#'   to \code{embed_query()}.  \code{NULL} (default) reads the
#'   \code{"embed_model"} graph attribute.
#' @param verbose Logical(1).  Print progress messages via
#'   \code{cli_inform()}.  Default \code{FALSE}.
#'
#' @return A named list with class \code{c("rrlm_context", "list")}:
#' \describe{
#'   \item{\code{nodes}}{Character vector of absorbed node names,
#'         relevance-ordered, seed first.}
#'   \item{\code{context_string}}{Character(1) assembled by
#'         \code{assemble_context_string()}.}
#'   \item{\code{tokens_used}}{Integer(1).  Approximate token count of
#'         \code{context_string}.}
#'   \item{\code{budget_tokens}}{Integer(1).  The budget that was used.}
#'   \item{\code{seed_node}}{Character(1).  The seed node name.}
#'   \item{\code{relevance_scores}}{Named numeric vector of relevance
#'         scores for every absorbed node (seed included).}
#' }
#'
#' @seealso [compute_relevance()], [build_node_context()],
#'   [assemble_context_string()], [update_task_weights()]
#' @export
#' @examples
#' \dontrun{
#' g   <- build_rrlm_graph("mypkg")
#' ctx <- query_context(g, "load training data", budget_tokens = 1000L)
#' cat(ctx$context_string)
#' }
query_context <- function(
  graph,
  query,
  seed_node = NULL,
  budget_tokens = 2000L,
  min_relevance = 0.1,
  max_nodes = 20L,
  method = NULL,
  verbose = FALSE
) {
  # ---- input validation -----------------------------------------------
  if (!inherits(graph, "igraph")) {
    cli::cli_abort("{.arg graph} must be an igraph / rrlm_graph object.")
  }
  if (!is.character(query) || length(query) != 1L) {
    cli::cli_abort("{.arg query} must be a single character string.")
  }
  budget_tokens <- as.integer(budget_tokens)
  max_nodes <- as.integer(max_nodes)

  if (igraph::vcount(graph) == 0L) {
    return(.empty_rrlm_context(budget_tokens))
  }

  # ---- embed query ----------------------------------------------------
  embed_model <- if (!is.null(method)) {
    method
  } else {
    igraph::graph_attr(graph, "embed_model")
  }

  query_vec <- tryCatch(
    embed_query(query, embed_model),
    error = function(e) numeric(0)
  )

  # ---- select seed node -----------------------------------------------
  v_names <- igraph::V(graph)$name
  v_types <- igraph::vertex_attr(graph, "node_type")

  if (is.null(seed_node)) {
    fn_idx <- which(!is.na(v_types) & v_types == "function")
    if (length(fn_idx) == 0L) {
      fn_idx <- seq_along(v_names)
    }

    pr_vals <- igraph::vertex_attr(graph, "pagerank")
    if (is.null(pr_vals) || all(is.na(pr_vals[fn_idx]))) {
      seed_node <- v_names[fn_idx[[1L]]]
    } else {
      seed_node <- v_names[fn_idx[which.max(pr_vals[fn_idx])]]
    }
  } else {
    if (!seed_node %in% v_names) {
      cli::cli_abort(
        "{.val {seed_node}} is not a vertex in {.arg graph}."
      )
    }
  }

  if (verbose) {
    cli::cli_inform("Seed node: {.val {seed_node}}")
  }

  # ---- seed context ---------------------------------------------------
  seed_ctx <- build_node_context(seed_node, graph, mode = "full")
  seed_tokens <- as.integer(ceiling(nchar(seed_ctx) / 4L))

  if (seed_tokens > budget_tokens) {
    seed_ctx <- substr(seed_ctx, 1L, budget_tokens * 4L)
    seed_tokens <- budget_tokens
  }

  tokens_used <- seed_tokens
  visited <- seed_node
  hits <- seed_node
  relevance_scores <- c(setNames(1.0, seed_node))

  # ---- BFS loop -----------------------------------------------
  frontier <- unique(igraph::V(graph)$name[
    igraph::neighbors(graph, seed_node, mode = "all")
  ])
  frontier <- setdiff(frontier, visited)

  nodes_added <- 0L

  while (
    tokens_used < budget_tokens &&
      length(frontier) > 0L &&
      nodes_added < max_nodes
  ) {
    # score every frontier node
    scores <- vapply(
      frontier,
      function(fn) {
        compute_relevance(fn, query_vec, visited, graph)
      },
      numeric(1L)
    )

    # sort descending
    ord <- order(scores, decreasing = TRUE)
    frontier <- frontier[ord]
    scores <- scores[ord]

    admitted <- FALSE
    for (k in seq_along(frontier)) {
      fn <- frontier[[k]]
      sc <- scores[[k]]

      if (sc < min_relevance) {
        break
      } # all remaining are below threshold

      node_ctx <- build_node_context(fn, graph, mode = "full")
      node_tokens <- as.integer(ceiling(nchar(node_ctx) / 4L))

      if (tokens_used + node_tokens <= budget_tokens) {
        tokens_used <- tokens_used + node_tokens
        visited <- c(visited, fn)
        hits <- c(hits, fn)
        relevance_scores <- c(relevance_scores, setNames(sc, fn))
        nodes_added <- nodes_added + 1L
        admitted <- TRUE

        if (verbose) {
          cli::cli_inform(
            "  + {.val {fn}} (score={round(sc,3)}, tokens={node_tokens})"
          )
        }

        # expand neighbours
        nb <- unique(igraph::V(graph)$name[
          igraph::neighbors(graph, fn, mode = "all")
        ])
        frontier <- unique(c(
          frontier[-k],
          setdiff(nb, visited)
        ))
        break
      }
    }

    if (!admitted) {
      # No node fits within the remaining budget -- stop
      break
    }
  }

  # ---- task trace update ----------------------------------------------
  graph <- tryCatch(
    update_task_weights(graph, useful_nodes = hits),
    error = function(e) graph
  )

  # ---- assemble context string ----------------------------------------
  context_string <- assemble_context_string(hits, graph, query)
  final_tokens <- as.integer(ceiling(nchar(context_string) / 4L))

  # Hard-enforce the budget on the final assembled output. assemble_context_string
  # adds section headers / formatting beyond the raw node contexts counted during
  # the BFS loop, so the assembled string can slightly exceed budget_tokens.
  if (final_tokens > budget_tokens) {
    context_string <- substr(context_string, 1L, budget_tokens * 4L)
    final_tokens <- budget_tokens
  }

  structure(
    list(
      nodes = hits,
      context_string = context_string,
      tokens_used = final_tokens,
      budget_tokens = budget_tokens,
      seed_node = seed_node,
      relevance_scores = relevance_scores
    ),
    class = c("rrlm_context", "list")
  )
}

# ---- rrlm_context S3 helpers ----------------------------------------

#' @keywords internal
.empty_rrlm_context <- function(budget_tokens) {
  structure(
    list(
      nodes = character(0),
      context_string = "",
      tokens_used = 0L,
      budget_tokens = budget_tokens,
      seed_node = NA_character_,
      relevance_scores = numeric(0)
    ),
    class = c("rrlm_context", "list")
  )
}

# ---- print.rrlm_context ----------------------------------------------

#' Print an rrlm_context object
#'
#' @param x An \code{rrlm_context} object.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @seealso [query_context()], [summary.rrlm_context()]
#' @examples
#' \dontrun{
#' g   <- build_rrlm_graph("mypkg")
#' ctx <- query_context(g, "load data")
#' print(ctx)
#' }
#' @export
print.rrlm_context <- function(x, ...) {
  cli::cli_h1("rrlm_context")
  cli::cli_bullets(c(
    "*" = "Seed node  : {.val {x$seed_node}}",
    "*" = "Nodes      : {length(x$nodes)} ({paste(utils::head(x$nodes, 5), collapse = ', ')}{if (length(x$nodes) > 5) ', ...' else ''})",
    "*" = "Tokens     : {x$tokens_used} / {x$budget_tokens}"
  ))
  invisible(x)
}

# ---- summary.rrlm_context --------------------------------------------

#' Summarise an rrlm_context object
#'
#' @param object An \code{rrlm_context} object.
#' @param ... Ignored.
#' @return \code{object}, invisibly.
#' @seealso [query_context()], [print.rrlm_context()]
#' @examples
#' \dontrun{
#' g   <- build_rrlm_graph("mypkg")
#' ctx <- query_context(g, "load data")
#' summary(ctx)
#' }
#' @export
summary.rrlm_context <- function(object, ...) {
  cli::cli_h1("rrlm_context summary")
  cli::cli_h2("Budget")
  cli::cli_bullets(c(
    "*" = "Tokens used : {object$tokens_used}",
    "*" = "Budget      : {object$budget_tokens}",
    "*" = "Utilisation : {round(100 * object$tokens_used / max(object$budget_tokens, 1L), 1)}%%"
  ))
  cli::cli_h2("Traversal")
  cli::cli_text("Seed: {.val {object$seed_node}}")
  if (length(object$relevance_scores) > 0L) {
    top <- sort(object$relevance_scores, decreasing = TRUE)
    top <- utils::head(top, 5L)
    cli::cli_text("Top-5 relevance scores:")
    for (nm in names(top)) {
      cli::cli_text("  {nm}: {round(top[[nm]], 4)}")
    }
  }
  invisible(object)
}
