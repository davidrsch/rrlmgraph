# R/rrlm_graph.R
# S3 methods for the "rrlm_graph" class
# Covers rrlmgraph issue #7 (Sprint 1)

# ---- print ---------------------------------------------------------------

#' Print a compact one-line summary of an rrlm_graph
#'
#' @param x An `rrlm_graph` object.
#' @param ... Ignored.
#' @return `x` invisibly.
#' @seealso [summary.rrlm_graph()], [plot.rrlm_graph()], [build_rrlm_graph()]
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' print(g)
#' }
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
#' @seealso [print.rrlm_graph()], [plot.rrlm_graph()], [build_rrlm_graph()]
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' summary(g)
#' }
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
#' Renders the top-`n_hubs` function nodes (by PageRank) using
#' [igraph::plot.igraph()].  Package and test-file nodes are included only
#' when they are connected to the selected hubs; their labels are suppressed
#' to avoid clutter.  Vertex size scales with relative PageRank importance.
#'
#' Node colours:
#' * User functions -- `"steelblue"`
#' * Package nodes  -- `"#C8D8E8"` (pale blue, smaller)
#' * Test files     -- `"seagreen3"`
#' * Other/unknown  -- `"lightyellow"`
#'
#' @param x An `rrlm_graph` object.
#' @param n_hubs Integer(1).  Number of top-PageRank *function* hub nodes to
#'   show.  Default `15`.
#' @param layout Function.  igraph layout function.  Auto-selects
#'   [igraph::layout_with_kk] for ≤ 20 nodes and
#'   [igraph::layout_with_fr] for larger sub-graphs when `NULL` (default).
#' @param vertex.label.cex Numeric(1).  Label size for function nodes.
#'   Default `0.75`.
#' @param edge.arrow.size Numeric(1).  Arrow size.  Default `0.3`.
#' @param ... Additional arguments forwarded to [igraph::plot.igraph()].
#' @return `x` invisibly.
#' @seealso [print.rrlm_graph()], [summary.rrlm_graph()], [build_rrlm_graph()]
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' plot(g)
#' plot(g, n_hubs = 10L)
#' }
#' @export
plot.rrlm_graph <- function(
  x,
  n_hubs = 15L,
  layout = NULL,
  vertex.label.cex = 0.75,
  edge.arrow.size = 0.3,
  ...
) {
  g <- x

  if (igraph::vcount(g) == 0L) {
    cli::cli_warn("Graph has no nodes -- nothing to plot.")
    return(invisible(g))
  }

  # -- Select top n_hubs *function* nodes by PageRank -------------------
  nt_all <- igraph::V(g)$node_type
  if (is.null(nt_all)) nt_all <- rep("function", igraph::vcount(g))
  fn_idx <- which(nt_all == "function")

  pr <- igraph::V(g)$pagerank
  if (is.null(pr)) pr <- rep(1 / igraph::vcount(g), igraph::vcount(g))
  pr <- as.numeric(pr)

  k <- min(as.integer(n_hubs), length(fn_idx))
  top_fn <- fn_idx[order(pr[fn_idx], decreasing = TRUE)[seq_len(k)]]

  # Include neighbour nodes (one hop) so edges have both endpoints
  nbrs <- unique(unlist(igraph::neighborhood(g, order = 1L, nodes = top_fn)))
  keep_idx <- unique(c(top_fn, nbrs))
  sub <- igraph::induced_subgraph(g, keep_idx)

  # -- Node colours by type ---------------------------------------------
  type_map <- c(
    "function" = "#4682B4",   # steelblue
    "package"  = "#C8D8E8",   # pale blue -- unobtrusive
    "testfile" = "#3CB371"    # seagreen3
  )
  nt_sub <- igraph::V(sub)$node_type
  if (is.null(nt_sub)) nt_sub <- rep("function", igraph::vcount(sub))
  colours <- ifelse(nt_sub %in% names(type_map), type_map[nt_sub], "#FFFACD")

  # -- Vertex sizes: scale by PageRank, pkg/test nodes smaller ----------
  pr_sub <- as.numeric(igraph::V(sub)$pagerank)
  pr_sub[is.na(pr_sub)] <- 0
  pr_range <- range(pr_sub)
  if (diff(pr_range) > 0) {
    scaled <- 6 + 14 * (pr_sub - pr_range[[1L]]) / diff(pr_range)
  } else {
    scaled <- rep(8, igraph::vcount(sub))
  }
  # shrink package/testfile nodes
  scaled[nt_sub %in% c("package", "testfile")] <-
    scaled[nt_sub %in% c("package", "testfile")] * 0.55

  # -- Labels: only for function nodes, bare name only ------------------
  labels <- ifelse(
    nt_sub == "function",
    vapply(igraph::V(sub)$name, function(nm) {
      tail(strsplit(nm, "::", fixed = TRUE)[[1L]], 1L)
    }, character(1L)),
    NA_character_
  )

  # -- Layout -----------------------------------------------------------
  if (is.null(layout)) {
    layout <- if (igraph::vcount(sub) <= 20L) {
      igraph::layout_with_kk
    } else {
      igraph::layout_with_fr
    }
  }
  coords <- layout(sub)

  # -- Plot -------------------------------------------------------------
  op <- graphics::par(mar = c(0, 0, 2.5, 0))
  on.exit(graphics::par(op), add = TRUE)

  igraph::plot.igraph(
    sub,
    layout        = coords,
    vertex.color  = colours,
    vertex.size   = scaled,
    vertex.label  = labels,
    vertex.label.cex    = vertex.label.cex,
    vertex.label.color  = "white",
    vertex.label.font   = 2L,          # bold
    vertex.frame.color  = "white",
    edge.arrow.size     = edge.arrow.size,
    edge.color          = "#88888866",  # semi-transparent grey
    edge.curved         = 0.15,
    main = paste0(
      igraph::graph_attr(g, "project_name") %||% "rrlm_graph",
      "  \u2014  top ", k, " hubs"
    ),
    ...
  )

  # -- Legend -----------------------------------------------------------
  present_types <- intersect(c("function", "package", "testfile"), nt_sub)
  legend_labels <- c(
    "function" = "user function",
    "package"  = "package dep.",
    "testfile" = "test file"
  )
  graphics::legend(
    "bottomleft",
    legend  = legend_labels[present_types],
    pch     = 21L,
    pt.bg   = type_map[present_types],
    col     = "white",
    pt.cex  = 1.4,
    cex     = 0.75,
    bty     = "n",
    text.col = "grey30"
  )

  invisible(g)
}
