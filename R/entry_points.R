# R/entry_points.R
# Dynamic entry-point detection for rrlm_graph objects.

# ---- detect_entry_points -------------------------------------------------

#' Detect entry-point function nodes in an rrlm graph
#'
#' Determines which function nodes represent the callable surface of an R
#' project.  The strategy depends on the detected project type:
#'
#' \describe{
#'   \item{\code{"package"}}{Parses the \file{NAMESPACE} file for
#'     \code{export(...)} lines.  Falls back to zero-CALLS-in-degree when
#'     \file{NAMESPACE} is absent or yields no matches.}
#'   \item{\code{"shiny"}}{Zero CALLS in-degree from project-internal
#'     \code{CALLS} edges: the Shiny framework (not user code) invokes
#'     \code{server} / \code{ui} directly.}
#'   \item{\code{"script"}, \code{"quarto"}, \code{"rmarkdown"}}{Same as
#'     Shiny: functions not called by any other project function are
#'     top-level entry points.}
#' }
#'
#' All candidates are first restricted to \code{scope_level == 0} nodes
#' (i.e. top-level function definitions).  If detection yields no results
#' for the given type, the fallback is all \code{scope_level == 0} function
#' nodes.
#'
#' @param graph An \code{rrlm_graph} / \code{igraph} object created by
#'   \code{build_rrlm_graph()}.
#' @param project The list returned by \code{detect_rproject()}.  Must
#'   contain at least \code{project$type} and \code{project$root}.
#'
#' @return Character vector of node names (\code{node_id} values) that are
#'   entry points.  May be empty only when the graph has no function nodes.
#'
#' @seealso [build_rrlm_graph()], [query_context()]
#' @export
#' @examples
#' \dontrun{
#' proj <- detect_rproject("/path/to/mypkg")
#' g    <- build_rrlm_graph("/path/to/mypkg")
#' eps  <- detect_entry_points(g, proj)
#' }
detect_entry_points <- function(graph, project) {
    if (!inherits(graph, "igraph")) {
        cli::cli_abort("{.arg graph} must be an igraph / rrlm_graph object.")
    }

    v_names <- igraph::V(graph)$name
    v_types <- igraph::vertex_attr(graph, "node_type")
    v_scope <- igraph::vertex_attr(graph, "scope_level")

    # Candidates: scope_level == 0 function nodes
    # (Old graphs without scope_level treat every function as top-level.)
    if (is.null(v_scope)) {
        fn_idx <- which(!is.na(v_types) & v_types == "function")
    } else {
        fn_idx <- which(
            !is.na(v_types) &
                v_types == "function" &
                !is.na(v_scope) &
                as.integer(v_scope) == 0L
        )
    }

    if (length(fn_idx) == 0L) {
        return(character(0))
    }

    fn_names <- v_names[fn_idx]
    proj_type <- if (is.list(project)) project$type else as.character(project)

    result <- switch(
        proj_type,
        "package" = .ep_package(fn_names, graph, project),
        "shiny" = .ep_zero_indegree(fn_names, graph),
        "script" = .ep_zero_indegree(fn_names, graph),
        "quarto" = .ep_zero_indegree(fn_names, graph),
        "rmarkdown" = .ep_zero_indegree(fn_names, graph),
        # Unknown type: zero in-degree heuristic
        .ep_zero_indegree(fn_names, graph)
    )

    # Fallback: all scope_level == 0 function nodes
    if (length(result) == 0L) {
        result <- fn_names
    }

    result
}

# ---- internal helpers -------------------------------------------------------

#' @keywords internal
# Package entry points: functions listed in NAMESPACE export() declarations.
# Falls back to zero-in-degree when NAMESPACE is absent or yields no matches.
.ep_package <- function(fn_names, graph, project) {
    root <- if (is.list(project)) project$root else "."
    ns_path <- file.path(root, "NAMESPACE")

    if (file.exists(ns_path)) {
        ns_lines <- tryCatch(
            readLines(ns_path, warn = FALSE),
            error = function(e) character(0)
        )

        # Match: export("name"), export(name), exportMethods("name") etc.
        # Captures the symbol name inside the outer parentheses.
        exported <- character(0)
        for (ln in ns_lines) {
            # Standard export() — one symbol per call
            m <- regmatches(
                ln,
                regexpr(
                    'export\\s*\\(\\s*"?([A-Za-z._][^"(),]*)"?\\s*\\)',
                    ln,
                    perl = TRUE
                )
            )
            if (length(m) > 0L) {
                inner <- sub(
                    '^export\\s*\\(\\s*"?',
                    "",
                    sub('"?\\s*\\)$', "", m[[1L]])
                )
                exported <- c(exported, trimws(inner))
            }
            # exportMethods()
            m2 <- regmatches(
                ln,
                regexpr(
                    'exportMethods\\s*\\(\\s*"?([A-Za-z._][^"(),]*)"?\\s*\\)',
                    ln,
                    perl = TRUE
                )
            )
            if (length(m2) > 0L) {
                inner2 <- sub(
                    '^exportMethods\\s*\\(\\s*"?',
                    "",
                    sub('"?\\s*\\)$', "", m2[[1L]])
                )
                exported <- c(exported, trimws(inner2))
            }
        }
        exported <- unique(exported[nzchar(exported)])

        if (length(exported) > 0L) {
            # node IDs are "dir/file_stem::fn_name" — extract the fn_name part
            node_fn_names <- sub("^.*::", "", fn_names)
            in_ns <- fn_names[node_fn_names %in% exported]
            if (length(in_ns) > 0L) {
                return(in_ns)
            }
        }
    }

    # Fallback when NAMESPACE absent or no export() matches
    .ep_zero_indegree(fn_names, graph)
}

#' @keywords internal
# Zero CALLS in-degree: nodes that are not called by any other project function.
.ep_zero_indegree <- function(fn_names, graph) {
    # Build a CALLS-only subgraph to isolate in-degree from project-internal edges.
    # IMPORTS / TESTS / SEMANTIC edges do not count as "being called".
    edge_types <- igraph::E(graph)$edge_type
    calls_edge_idx <- which(!is.na(edge_types) & edge_types == "CALLS")

    if (length(calls_edge_idx) == 0L) {
        # No CALLS edges: every function is an entry point (e.g. unconnected graph)
        return(fn_names)
    }

    calls_g <- igraph::subgraph_from_edges(
        graph,
        calls_edge_idx,
        delete.vertices = FALSE
    )
    # in-degree per vertex, named by vertex name
    in_deg <- igraph::degree(calls_g, mode = "in")

    fn_names[in_deg[fn_names] == 0L]
}
