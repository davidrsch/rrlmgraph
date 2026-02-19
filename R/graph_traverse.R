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

# ---- update_task_weights (stub) -------------------------------------

#' Update task-trace weights on graph vertices
#'
#' Record that a set of nodes was useful for a task, increasing their
#' \code{task_trace_weight} vertex attribute so they rank higher in
#' future \code{compute_relevance()} calls.
#'
#' The full learning implementation (issue \#13) writes history to
#' SQLite.  This Sprint-2 stub applies an in-memory exponential
#' moving-average update and returns the modified graph.
#'
#' @param graph An \code{rrlm_graph} / \code{igraph} object.
#' @param useful_nodes Character vector of node names that were
#'   helpful for the current task.
#' @param alpha Numeric(1).  EMA learning rate in \eqn{(0, 1)}.
#'   Default \eqn{0.3}.
#' @param decay Numeric(1).  Multiplicative decay applied to all
#'   \emph{other} nodes so weights stay bounded.  Default \eqn{0.99}.
#'
#' @return The modified \code{graph} with updated \code{task_trace_weight}
#'   vertex attributes.
#' @export
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' g <- update_task_weights(g, useful_nodes = c("utils::load_data"))
#' }
update_task_weights <- function(
  graph,
  useful_nodes,
  alpha = 0.3,
  decay = 0.99
) {
  # Initialise attribute if absent
  cur <- igraph::V(graph)$task_trace_weight
  if (is.null(cur)) {
    igraph::V(graph)$task_trace_weight <- rep(0.5, igraph::vcount(graph))
    cur <- igraph::V(graph)$task_trace_weight
  }
  cur <- as.numeric(cur)
  cur[is.na(cur)] <- 0.5

  # Decay all nodes
  cur <- cur * decay

  # Boost useful nodes via EMA towards 1.0
  idx <- match(useful_nodes, igraph::V(graph)$name)
  idx <- idx[!is.na(idx)]
  if (length(idx) > 0L) {
    cur[idx] <- cur[idx] * (1 - alpha) + 1.0 * alpha
  }

  # Clamp to [0, 1]
  cur <- pmin(1, pmax(0, cur))
  igraph::V(graph)$task_trace_weight <- cur
  graph
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
