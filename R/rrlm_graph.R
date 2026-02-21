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
  cat(sprintf(
    "<rrlm_graph> %s | %d nodes | %d edges | embed: %s\n",
    pn,
    n_v,
    n_e,
    em
  ))
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

  cat(sprintf("=== rrlm_graph: %s ===\n", pn))
  cat(sprintf("Root:  %s\n", root))
  cat(sprintf("Built: %s\n", ba))
  if (!is.na(bt)) {
    cat(sprintf("Build time: %s s\n", round(bt, 2)))
  }

  # -- Node counts by type -----------------------------------------------
  cat(sprintf("\nNodes (%d total):\n", igraph::vcount(g)))
  if (igraph::vcount(g) > 0L) {
    nt <- igraph::V(g)$node_type
    nt <- if (is.null(nt)) rep(NA_character_, igraph::vcount(g)) else nt
    tbl <- sort(table(nt), decreasing = TRUE)
    for (nm in names(tbl)) {
      cat(sprintf("  %s: %d\n", nm, tbl[[nm]]))
    }
  }

  # -- Edge counts by type -----------------------------------------------
  cat(sprintf("\nEdges (%d total):\n", igraph::ecount(g)))
  if (igraph::ecount(g) > 0L) {
    et <- igraph::E(g)$edge_type
    et <- if (is.null(et)) rep(NA_character_, igraph::ecount(g)) else et
    tbl <- sort(table(et), decreasing = TRUE)
    for (nm in names(tbl)) {
      cat(sprintf("  %s: %d\n", nm, tbl[[nm]]))
    }
  }

  # -- Top-5 PageRank ----------------------------------------------------
  cat("\nTop-5 nodes by PageRank:\n")
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
      cat(sprintf("  %d. %s (%s)\n", i, nms[[i]], vals[[i]]))
    }
  }

  # -- Metadata ----------------------------------------------------------
  cat(sprintf("\nMetadata:\n"))
  cat(sprintf("  Embed method: %s\n", em))
  cat(sprintf("  Cache path:   %s\n", cp))

  invisible(g)
}

# ---- plot ----------------------------------------------------------------

#' Plot an rrlm_graph
#'
#' Renders the top-`n_hubs` function nodes (by PageRank) and their
#' one-hop neighbours as an interactive **Graphviz** widget via
#' [DiagrammeR::grViz()].  Nodes are grouped into dashed sub-graph boxes by
#' source file and coloured by node type; node width scales with relative
#' PageRank importance.  In an interactive session the widget appears in the
#' RStudio Viewer or a browser tab, supporting pan and zoom.
#'
#' Node colours:
#' * User functions -- `"#4682B4"` (steelblue)
#' * Package nodes  -- `"#C8D8E8"` (pale blue, smaller)
#' * Test files     -- `"#3CB371"` (seagreen3)
#'
#' @param x An `rrlm_graph` object.
#' @param n_hubs Integer(1).  Number of top-PageRank *function* hub nodes to
#'   show.  Default `15L`.
#' @param layout Character(1).  Graphviz layout engine passed to the DOT
#'   `layout` attribute.  One of `"dot"` (hierarchical, default), `"neato"`,
#'   `"fdp"`, `"sfdp"`, or `"circo"`.
#' @param file Character(1) or `NULL`.  Optional output file path.
#'   * `.html` -- saved via [htmlwidgets::saveWidget()] (requires the
#'     **htmlwidgets** package).
#'   * `.png`, `.pdf`, `.svg` -- rendered by [webshot2::webshot()] after
#'     writing a temporary HTML file (requires both **htmlwidgets** and
#'     **webshot2**).
#' @param width,height Integer(1).  Pixel dimensions for raster image export
#'   via `file`.  Defaults `1400L` x `900L`.
#' @param ... Ignored; kept for S3 dispatch compatibility.
#' @return When `file` is `NULL` (default), an `htmlwidget` from
#'   [DiagrammeR::grViz()] is returned visibly so it prints in the viewer.
#'   When `file` is supplied, `x` is returned invisibly.
#' @seealso [print.rrlm_graph()], [summary.rrlm_graph()], [build_rrlm_graph()]
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' plot(g)
#' plot(g, n_hubs = 10L, layout = "neato")
#' plot(g, file = "graph.html")
#' plot(g, file = "graph.png", width = 1600L, height = 1000L)
#' }
#' @export
plot.rrlm_graph <- function(
  x,
  n_hubs = 15L,
  layout = c("dot", "neato", "fdp", "sfdp", "circo"),
  file = NULL,
  width = 1400L,
  height = 900L,
  ...
) {
  g <- x
  layout <- match.arg(layout)

  if (igraph::vcount(g) == 0L) {
    cli::cli_inform("Graph has no nodes -- nothing to plot.")
    return(invisible(x))
  }

  # -- Select top n_hubs *function* nodes by PageRank -------------------
  nt_all <- igraph::vertex_attr(g, "node_type")
  if (is.null(nt_all)) {
    nt_all <- rep("function", igraph::vcount(g))
  }
  fn_idx <- which(nt_all == "function")

  pr <- igraph::vertex_attr(g, "pagerank")
  if (is.null(pr)) {
    pr <- rep(1 / igraph::vcount(g), igraph::vcount(g))
  }
  pr <- as.numeric(pr)

  k <- min(as.integer(n_hubs), length(fn_idx))
  top_fn <- fn_idx[order(pr[fn_idx], decreasing = TRUE)[seq_len(k)]]
  nbrs <- unique(unlist(igraph::neighborhood(g, order = 1L, nodes = top_fn)))
  sub <- igraph::induced_subgraph(g, unique(c(top_fn, nbrs)))

  if (igraph::vcount(sub) <= 1L) {
    cli::cli_inform(c(
      "i" = "Sub-graph has only {igraph::vcount(sub)} node(s) -- skipping plot."
    ))
    return(invisible(x))
  }

  # -- Build DOT string and render widget -------------------------------
  project_nm <- igraph::graph_attr(g, "project_name") %||% "rrlm_graph"
  dot <- .rrlmgraph_to_dot(sub, k, project_nm, layout)
  widget <- DiagrammeR::grViz(dot)

  # -- Export / return --------------------------------------------------
  if (!is.null(file)) {
    ext <- tolower(tools::file_ext(file))
    rlang::check_installed(
      "htmlwidgets",
      reason = "to save an rrlm_graph plot to a file"
    )
    if (ext == "html") {
      htmlwidgets::saveWidget(
        widget,
        normalizePath(file, mustWork = FALSE),
        selfcontained = TRUE
      )
    } else if (ext %in% c("png", "pdf", "svg")) {
      rlang::check_installed(
        "webshot2",
        reason = "to export an rrlm_graph plot as an image"
      )
      tmp <- tempfile(fileext = ".html")
      on.exit(unlink(tmp), add = TRUE)
      htmlwidgets::saveWidget(widget, tmp, selfcontained = TRUE)
      webshot2::webshot(
        tmp,
        file = file,
        vwidth = width,
        vheight = height,
        delay = 0.5
      )
    } else {
      cli::cli_abort(c(
        "Unsupported file extension {.val {ext}}.",
        "i" = "Use one of: .html, .png, .pdf, .svg"
      ))
    }
    return(invisible(x))
  }

  widget
}

# ---- internal: Graphviz DOT string builder ---------------------------

#' Build a Graphviz DOT string from an rrlm_graph subgraph
#'
#' @param sub          igraph subgraph (already filtered to hubs + neighbours)
#' @param k            Number of hub nodes shown (used in title)
#' @param project_name Character(1) project name for the graph title
#' @param layout       Character(1) Graphviz layout engine
#' @return Character(1) DOT language string
#' @noRd
.rrlmgraph_to_dot <- function(sub, k, project_name, layout = "dot") {
  nt_sub <- igraph::vertex_attr(sub, "node_type")
  pr_sub <- as.numeric(igraph::vertex_attr(sub, "pagerank"))
  names_sub <- igraph::vertex_attr(sub, "name")

  if (is.null(nt_sub)) {
    nt_sub <- rep("function", igraph::vcount(sub))
  }
  if (is.null(names_sub)) {
    names_sub <- paste0("n", seq_len(igraph::vcount(sub)))
  }
  pr_sub[is.na(pr_sub)] <- 0

  # -- Type-to-colour maps -----------------------------------------------
  type_fill <- c(
    "function" = "#4682B4",
    "package" = "#C8D8E8",
    "testfile" = "#3CB371"
  )
  type_font <- c(
    "function" = "white",
    "package" = "#444444",
    "testfile" = "white"
  )

  # -- Node width scaled by PageRank (0.4 - 2.0 in); pkg/test fixed -----
  pr_range <- range(pr_sub)
  widths <- if (diff(pr_range) > 0) {
    0.4 + 1.6 * (pr_sub - pr_range[[1L]]) / diff(pr_range)
  } else {
    rep(0.8, length(pr_sub))
  }
  widths[nt_sub %in% c("package", "testfile")] <- 0.3

  # -- Bare labels (strip "file::" prefix) + DOT escaping ---------------
  bare_labels <- vapply(
    names_sub,
    function(nm) {
      lbl <- tail(strsplit(nm, "::", fixed = TRUE)[[1L]], 1L)
      lbl <- gsub("\\", "\\\\", lbl, fixed = TRUE)
      gsub('"', '\\"', lbl, fixed = TRUE)
    },
    character(1L)
  )

  # -- Safe DOT node IDs (n1, n2, ...) ----------------------------------
  node_ids <- paste0("n", seq_along(names_sub))
  name_to_id <- stats::setNames(node_ids, names_sub)

  # -- Source-file prefix for cluster grouping (function nodes only) ----
  file_group <- vapply(
    names_sub,
    function(nm) {
      parts <- strsplit(nm, "::", fixed = TRUE)[[1L]]
      fg <- if (length(parts) >= 2L) parts[[1L]] else "other"
      gsub("[^A-Za-z0-9_]", "_", fg) # valid DOT subgraph id
    },
    character(1L)
  )

  # -- Node attribute lines ---------------------------------------------
  node_defs <- vapply(
    seq_along(names_sub),
    function(i) {
      nt <- nt_sub[[i]]
      fill <- type_fill[[nt]]
      if (is.na(fill) || is.null(fill)) {
        fill <- "#FFFACD"
      }
      fc <- type_font[[nt]]
      if (is.na(fc) || is.null(fc)) {
        fc <- "#222222"
      }
      sprintf(
        '  %s [label="%s", fillcolor="%s", fontcolor="%s", width=%.2f];',
        node_ids[[i]],
        bare_labels[[i]],
        fill,
        fc,
        round(widths[[i]], 2L)
      )
    },
    character(1L)
  )

  # -- Edge lines (edge_type is UPPERCASE in rrlmgraph) -----------------
  el <- igraph::as_data_frame(sub, what = "edges")
  edge_defs <- character(0L)
  if (nrow(el) > 0L) {
    et <- if ("edge_type" %in% names(el)) {
      toupper(el$edge_type)
    } else {
      rep("CALLS", nrow(el))
    }
    ec <- ifelse(
      et == "CALLS",
      "#888888",
      ifelse(
        et == "IMPORTS",
        "#4682B4",
        ifelse(et == "TESTS", "#3CB371", "#888888")
      )
    )
    fids <- ifelse(
      el$from %in% names(name_to_id),
      name_to_id[el$from],
      NA_character_
    )
    tids <- ifelse(
      el$to %in% names(name_to_id),
      name_to_id[el$to],
      NA_character_
    )
    ok <- !is.na(fids) & !is.na(tids)
    if (any(ok)) {
      edge_defs <- sprintf(
        '  %s -> %s [color="%s"];',
        fids[ok],
        tids[ok],
        ec[ok]
      )
    }
  }

  # -- Cluster subgraphs (function nodes only, grouped by source file) --
  fn_mask <- nt_sub == "function"
  clusters <- split(which(fn_mask), file_group[fn_mask])
  cluster_blocks <- vapply(
    names(clusters),
    function(fg) {
      idxs <- clusters[[fg]]
      inner <- paste(node_ids[idxs], collapse = "; ")
      fg_lbl <- gsub("_", ".", fg, fixed = TRUE) # restore . for display
      sprintf(
        '  subgraph cluster_%s {\n    label="%s.R";\n    style=dashed;\n    color="#AAAAAA";\n    fontcolor="#666666";\n    fontsize=10;\n    %s;\n  }',
        fg,
        fg_lbl,
        inner
      )
    },
    character(1L)
  )

  # -- Assemble ---------------------------------------------------------
  title <- paste0(project_name, "  \u2014  top ", k, " hubs")
  title <- gsub("\\", "\\\\", title, fixed = TRUE)
  title <- gsub('"', '\\"', title, fixed = TRUE)

  paste0(
    'digraph rrlmgraph {\n',
    sprintf(
      '  graph [layout=%s, rankdir=LR, fontname="Helvetica", label="%s", labelloc=t, fontsize=14, splines=ortho, nodesep=0.5, ranksep=0.8];\n',
      layout,
      title
    ),
    '  node  [shape=box, style="rounded,filled", fontname="Helvetica", fontsize=10, penwidth=0.5, margin=0.1];\n',
    '  edge  [arrowsize=0.5];\n\n',
    paste(cluster_blocks, collapse = "\n\n"),
    "\n\n",
    paste(node_defs, collapse = "\n"),
    "\n\n",
    paste(edge_defs, collapse = "\n"),
    "\n",
    '}\n'
  )
}
