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
#'           \item Compute its token cost (\code{.count_tokens()}).
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
  seed_tokens <- .count_tokens(seed_ctx)

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

      # Estimate token cost using "compressed" mode for all BFS-discovered
      # supporting nodes.  The seed node is already accounted for above the
      # loop in "full" mode.  Using "full" here (the previous behaviour, fixed
      # in #48) over-counted the first supporting node by ~3x causing premature
      # BFS termination.  rrlmgraph#103: always "compressed" inside the loop.
      cost_mode <- "compressed"
      node_ctx <- build_node_context(fn, graph, mode = cost_mode)
      node_tokens <- .count_tokens(node_ctx)

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
  final_tokens <- .count_tokens(context_string)

  # Hard-enforce the budget on the final assembled output. assemble_context_string
  # adds section headers / formatting beyond the raw node contexts counted during
  # the BFS loop, so the assembled string can slightly exceed budget_tokens.
  # Truncate at the last newline before the limit so we never split mid-line
  # (which could produce malformed code blocks / invalid UTF-8 sequences).
  # NOTE: uses 3.5 chars/token to match context_assemble.R and MCP TypeScript
  # (rrlmgraph#114: was previously 4 chars/token, inconsistent with everything else).
  if (final_tokens > budget_tokens) {
    char_limit <- as.integer(ceiling(budget_tokens * 3.5))
    truncated <- substr(context_string, 1L, char_limit)
    last_nl <- max(c(0L, gregexpr("\n", truncated, fixed = TRUE)[[1L]]))
    context_string <- if (last_nl > 0L) {
      substr(context_string, 1L, last_nl)
    } else {
      truncated
    }
    cli::cli_warn(
      "Context truncated to {last_nl} chars to fit budget ({budget_tokens} tokens)."
    )
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
