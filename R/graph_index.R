# R/graph_index.R
# Incremental graph update utilities for rrlmgraph.
# Covers rrlmgraph issue #17 (Sprint 3).

# ---- update_graph_incremental ----------------------------------------

#' Incrementally update the graph for changed files
#'
#' Re-parses only the files that have changed and merges the resulting nodes
#' and edges back into the existing graph, avoiding a full rebuild.  PageRank
#' and embeddings are recomputed only for the affected portion of the graph,
#' making single-file updates substantially faster than [build_rrlm_graph()].
#'
#' @param graph An \code{rrlm_graph} / \code{igraph} object previously built
#'   by [build_rrlm_graph()].
#' @param changed_files Character vector of absolute file paths that have
#'   been added, edited, or deleted since the last full build.
#' @param embed_method Character(1) or \code{NULL}.  Embedding method passed
#'   to [embed_nodes()].  \code{NULL} (default) uses the method stored in
#'   \code{graph_attr(graph, "embed_method")}, falling back to
#'   \code{"tfidf"}.
#' @param verbose Logical(1).  Emit progress messages via \pkg{cli}.
#'   Default \code{FALSE}.
#'
#' @return The updated \code{rrlm_graph} object.  The graph retains all
#'   original vertex / edge attributes; only nodes from \code{changed_files}
#'   are touched.
#'
#' @seealso [build_rrlm_graph()], [build_call_edges()], [embed_nodes()],
#'   [save_graph_cache()]
#' @export
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' # After editing R/utils.R:
#' g <- update_graph_incremental(g, changed_files = "mypkg/R/utils.R")
#' }
update_graph_incremental <- function(
  graph,
  changed_files,
  embed_method = NULL,
  verbose = FALSE
) {
  # ---------- 1. Validate inputs ----------------------------------------
  if (!inherits(graph, "igraph")) {
    cli::cli_abort("{.arg graph} must be an igraph / rrlm_graph object.")
  }
  if (!is.character(changed_files) || length(changed_files) == 0L) {
    cli::cli_abort("{.arg changed_files} must be a non-empty character vector.")
  }
  changed_files <- unique(changed_files)

  if (verbose) {
    cli::cli_h2("Incremental graph update")
    cli::cli_inform("Changed files: {.file {changed_files}}")
  }

  # ---------- 2. Remove nodes from changed files ------------------------
  all_files <- igraph::vertex_attr(graph, "file")
  changed_files_norm <- normalizePath(changed_files, winslash = "/",
                                      mustWork = FALSE)
  all_files_norm <- normalizePath(all_files %||% character(0),
                                  winslash = "/", mustWork = FALSE)

  stale_idx <- which(all_files_norm %in% changed_files_norm)

  if (length(stale_idx) > 0L) {
    if (verbose) {
      stale_names <- igraph::V(graph)$name[stale_idx]
      cli::cli_inform("Removing {length(stale_idx)} stale node(s).")
    }
    graph <- igraph::delete_vertices(graph, stale_idx)
  }

  # ---------- 3. Re-parse changed files (skip deleted ones) -------------
  extant_files <- changed_files[file.exists(changed_files)]

  new_nodes <- list()
  if (length(extant_files) > 0L) {
    if (verbose) {
      cli::cli_inform("Re-parsing {length(extant_files)} file(s).")
    }
    new_nodes <- extract_function_nodes(extant_files)
  }

  if (length(new_nodes) == 0L) {
    if (verbose) {
      cli::cli_inform("No new nodes; finalising graph.")
    }
    graph <- .recompute_pagerank(graph, verbose)
    graph <- .maybe_save_cache(graph, verbose)
    return(graph)
  }

  # ---------- 4. Build new call and import edges ------------------------
  # Merge with existing node records to detect cross-file calls
  existing_names <- igraph::V(graph)$name
  existing_nodes <- .vertices_as_node_list(graph)

  all_nodes_for_edges <- c(existing_nodes, new_nodes)
  new_call_edges <- build_call_edges(all_nodes_for_edges)

  # Keep only edges where at least one endpoint is a new node
  new_node_ids <- vapply(new_nodes, `[[`, character(1), "node_id")
  is_new_edge <- new_call_edges$from %in% new_node_ids |
    new_call_edges$to %in% new_node_ids
  new_call_edges <- new_call_edges[is_new_edge, , drop = FALSE]

  # Similarly build import edges for new nodes
  new_import_edges <- build_import_edges(new_nodes)

  # ---------- 5. Embed new nodes ----------------------------------------
  method <- embed_method %||%
    igraph::graph_attr(graph, "embed_method") %||%
    "tfidf"

  if (verbose) {
    cli::cli_inform(paste0("Embedding ", length(new_nodes), " new node(s) ",
                           "using method '", method, "'."))
  }

  # Gather existing corpus so incremental embedding is consistent
  existing_corpus <- igraph::vertex_attr(graph, "doc")
  existing_corpus <- existing_corpus[!is.null(existing_corpus)]

  embed_result <- embed_nodes(
    new_nodes,
    method = method,
    existing_corpus = if (length(existing_corpus) > 0L) existing_corpus else NULL
  )

  # ---------- 6. Merge new vertices into graph --------------------------
  new_v_df <- .nodes_to_vertex_df(new_nodes, embed_result)
  new_e_df  <- rbind(new_call_edges, new_import_edges)

  # Add new vertices
  graph <- igraph::add_vertices(
    graph,
    nv     = nrow(new_v_df),
    attr   = as.list(new_v_df)
  )

  # Add new edges (filter to node_ids present in graph)
  if (nrow(new_e_df) > 0L) {
    g_names <- igraph::V(graph)$name
    valid_mask <- new_e_df$from %in% g_names & new_e_df$to %in% g_names
    new_e_df <- new_e_df[valid_mask, , drop = FALSE]

    if (nrow(new_e_df) > 0L) {
      from_idx <- match(new_e_df$from, g_names)
      to_idx   <- match(new_e_df$to, g_names)
      edge_vec <- as.vector(rbind(from_idx, to_idx))
      graph <- igraph::add_edges(graph, edge_vec,
                                 weight = new_e_df$weight %||%
                                   rep(1, nrow(new_e_df)))
    }
  }

  if (verbose) {
    cli::cli_inform(paste0("Graph now has ", igraph::vcount(graph),
                           " nodes, ", igraph::ecount(graph), " edges."))
  }

  # ---------- 7. Recompute PageRank -------------------------------------
  graph <- .recompute_pagerank(graph, verbose)

  # ---------- 8. Persist cache if project root available ---------------
  graph <- .maybe_save_cache(graph, verbose)

  graph
}

# ---- internal helpers ------------------------------------------------

#' @keywords internal
.recompute_pagerank <- function(graph, verbose = FALSE) {
  if (igraph::vcount(graph) == 0L) {
    return(graph)
  }
  if (verbose) cli::cli_inform("Recomputing PageRank.")
  pr <- igraph::page_rank(graph, directed = TRUE)$vector
  igraph::V(graph)$pagerank <- as.numeric(pr)
  graph
}

#' @keywords internal
.maybe_save_cache <- function(graph, verbose = FALSE) {
  root <- igraph::graph_attr(graph, "project_root") %||% NULL
  if (!is.null(root) && nchar(root) > 0L) {
    if (verbose) cli::cli_inform("Persisting cache to {.path {root}}.")
    tryCatch(
      save_graph_cache(graph),
      error = function(e) {
        cli::cli_warn("Cache save failed: {conditionMessage(e)}")
      }
    )
  }
  graph
}

#' Convert vertex attributes of a graph back into a minimal node list
#' sufficient for edge-building.
#' @keywords internal
.vertices_as_node_list <- function(graph) {
  if (igraph::vcount(graph) == 0L) {
    return(list())
  }
  names_v  <- igraph::V(graph)$name
  calls_v  <- igraph::vertex_attr(graph, "calls_list")
  pkg_v    <- igraph::vertex_attr(graph, "pkg")

  lapply(seq_along(names_v), function(i) {
    list(
      node_id    = names_v[[i]],
      name       = sub("^.*::", "", names_v[[i]]),
      calls_list = calls_v[[i]] %||% character(0),
      pkg        = pkg_v[[i]] %||% NA_character_
    )
  })
}

#' Convert a node list + embed result into a data.frame for add_vertices
#' @keywords internal
.nodes_to_vertex_df <- function(nodes, embed_result) {
  n <- length(nodes)
  node_ids  <- vapply(nodes, `[[`, character(1), "node_id")
  node_nm   <- vapply(nodes, `[[`, character(1), "name")
  node_type <- vapply(nodes, function(nd) nd$node_type  %||% "function",
                      character(1))
  node_file <- vapply(nodes, function(nd) nd$file       %||% NA_character_,
                      character(1))
  node_pkg  <- vapply(nodes, function(nd) nd$pkg        %||% NA_character_,
                      character(1))
  node_doc  <- vapply(nodes, function(nd) nd$doc        %||% "",
                      character(1))

  # Embeddings: list-column
  mat <- embed_result$matrix
  emb_list <- if (!is.null(mat) && nrow(mat) == n) {
    lapply(seq_len(n), function(i) mat[i, ])
  } else {
    rep(list(NULL), n)
  }

  data.frame(
    name              = node_ids,
    label             = node_nm,
    node_type         = node_type,
    file              = node_file,
    pkg               = node_pkg,
    doc               = node_doc,
    task_trace_weight = rep(0.5, n),
    pagerank          = rep(0, n),
    stringsAsFactors  = FALSE
  ) -> df

  # Attach embeddings as list-column
  df$embedding <- emb_list
  df
}
