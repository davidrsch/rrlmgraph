# R/rrlm_graph.R
# S3 methods for the "rrlm_graph" class
# Covers rrlmgraph issue #7 (Sprint 1)

# ---- print ---------------------------------------------------------------

#' Print a compact one-line summary of an rrlm_graph
#'
#' @param x An `rrlm_graph` object.
#' @param ... Ignored.
#' @return `x` invisibly.
#' @export
print.rrlm_graph <- function(x, ...) {
  n_v <- igraph::vcount(x)
  n_e <- igraph::ecount(x)
  pn <- igraph::graph_attr(x, "project_name") %||% "?"
  em <- igraph::graph_attr(x, "embed_method") %||% "?"
  cli::cli_text(
    "<rrlm_graph> {.val {pn}} | {n_v} nodes | {n_e} edges | embed: {em}"
  )
  invisible(x)
}

# ---- summary -------------------------------------------------------------

#' Detailed summary of an rrlm_graph
#'
#' Prints node counts by type, edge counts by type, the top-5 nodes by
#' PageRank centrality, and selected graph metadata (embed method, cache
#' path, build time).
#'
#' @param object An `rrlm_graph` object.
#' @param ... Ignored.
#' @return `object` invisibly.
#' @export
summary.rrlm_graph <- function(object, ...) {
  g <- object

  pn <- igraph::graph_attr(g, "project_name") %||% "unknown"
  root <- igraph::graph_attr(g, "project_root") %||% ""
  em <- igraph::graph_attr(g, "embed_method") %||% "?"
  cp <- igraph::graph_attr(g, "cache_path") %||% "(not cached)"
  bt <- igraph::graph_attr(g, "build_time") %||% NA_real_
  ba <- igraph::graph_attr(g, "build_at") %||% "?"

  cli::cli_h1("rrlm_graph: {pn}")
  cli::cli_text("Root: {.path {root}}")
  cli::cli_text("Built: {ba}")
  if (!is.na(bt)) {
    cli::cli_text("Build time: {round(bt, 2)} s")
  }

  # -- Node counts by type -----------------------------------------------
  cli::cli_h2("Nodes ({igraph::vcount(g)} total)")
  if (igraph::vcount(g) > 0L) {
    nt <- igraph::V(g)$node_type
    nt <- if (is.null(nt)) rep(NA_character_, igraph::vcount(g)) else nt
    tbl <- sort(table(nt), decreasing = TRUE)
    for (nm in names(tbl)) {
      cli::cli_bullets(c(" " = "{nm}: {tbl[[nm]]}"))
    }
  }

  # -- Edge counts by type -----------------------------------------------
  cli::cli_h2("Edges ({igraph::ecount(g)} total)")
  if (igraph::ecount(g) > 0L) {
    et <- igraph::E(g)$edge_type
    et <- if (is.null(et)) rep(NA_character_, igraph::ecount(g)) else et
    tbl <- sort(table(et), decreasing = TRUE)
    for (nm in names(tbl)) {
      cli::cli_bullets(c(" " = "{nm}: {tbl[[nm]]}"))
    }
  }

  # -- Top-5 PageRank ----------------------------------------------------
  cli::cli_h2("Top-5 nodes by PageRank")
  if (igraph::vcount(g) > 0L) {
    pr <- igraph::V(g)$pagerank
    if (is.null(pr)) {
      pr <- rep(0, igraph::vcount(g))
    }
    pr <- as.numeric(pr)
    k <- min(5L, length(pr))
    idx <- order(pr, decreasing = TRUE)[seq_len(k)]
    nms <- igraph::V(g)$name[idx]
    vals <- round(pr[idx], 6)
    for (i in seq_len(k)) {
      cli::cli_bullets(c(" " = "{i}. {nms[[i]]} ({vals[[i]]})"))
    }
  }

  # -- Metadata ----------------------------------------------------------
  cli::cli_h2("Metadata")
  cli::cli_bullets(c(
    " " = "Embed method: {em}",
    " " = "Cache path:   {cp}"
  ))

  invisible(g)
}

# ---- plot ----------------------------------------------------------------

#' Plot an rrlm_graph
#'
#' Renders the top-`n_hubs` nodes (by PageRank) using [igraph::plot.igraph()].
#' Node colour reflects the node type:
#' * User functions — `"steelblue"`
#' * Package nodes  — `"grey70"`
#' * Test files     — `"seagreen3"`
#' * Other/unknown  — `"lightyellow"`
#'
#' @param x An `rrlm_graph` object.
#' @param n_hubs Integer(1).  Number of top-PageRank hub nodes to include in
#'   the sub-graph.  Default `30`.
#' @param layout Function.  igraph layout function.  Defaults to
#'   [igraph::layout_with_fr].
#' @param vertex.size Numeric(1).  Vertex size passed to [igraph::plot.igraph()].
#'   Default `8`.
#' @param vertex.label.cex Numeric(1).  Label size.  Default `0.7`.
#' @param edge.arrow.size Numeric(1).  Arrow size.  Default `0.4`.
#' @param ... Additional arguments forwarded to [igraph::plot.igraph()].
#' @return `x` invisibly.
#' @export
plot.rrlm_graph <- function(
  x,
  n_hubs = 30L,
  layout = igraph::layout_with_fr,
  vertex.size = 8,
  vertex.label.cex = 0.7,
  edge.arrow.size = 0.4,
  ...
) {
  g <- x

  if (igraph::vcount(g) == 0L) {
    cli::cli_warn("Graph has no nodes — nothing to plot.")
    return(invisible(g))
  }

  # -- Select top n_hubs by PageRank ------------------------------------
  pr <- igraph::V(g)$pagerank
  if (is.null(pr)) {
    pr <- rep(0, igraph::vcount(g))
  }
  pr <- as.numeric(pr)
  k <- min(as.integer(n_hubs), igraph::vcount(g))
  idx <- order(pr, decreasing = TRUE)[seq_len(k)]
  sub <- igraph::induced_subgraph(g, idx)

  # -- Node colours by type ---------------------------------------------
  type_map <- c(
    "function" = "steelblue",
    "package" = "grey70",
    "testfile" = "seagreen3"
  )
  nt <- igraph::V(sub)$node_type
  if (is.null(nt)) {
    nt <- rep(NA_character_, igraph::vcount(sub))
  }
  colours <- ifelse(nt %in% names(type_map), type_map[nt], "lightyellow")

  # -- Node labels (short: last component of "::" id) -------------------
  labels <- vapply(
    igraph::V(sub)$name,
    function(nm) {
      parts <- strsplit(nm, "::", fixed = TRUE)[[1L]]
      tail(parts, 1L)
    },
    character(1)
  )

  igraph::plot.igraph(
    sub,
    layout = layout,
    vertex.color = colours,
    vertex.label = labels,
    vertex.size = vertex.size,
    vertex.label.cex = vertex.label.cex,
    edge.arrow.size = edge.arrow.size,
    main = paste0(
      "rrlm_graph: ",
      igraph::graph_attr(g, "project_name") %||% ""
    ),
    ...
  )

  invisible(g)
}
