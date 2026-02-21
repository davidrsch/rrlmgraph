# tests/testthat/test-s3-methods.R
# Unit tests for print.rrlm_graph, summary.rrlm_graph, plot.rrlm_graph
# Uses a lightweight synthetic rrlm_graph, no external bench fixture required.

skip_if_not_installed("igraph")

# ---- fixture: minimal synthetic rrlm_graph --------------------------
#
# We construct an igraph by hand and tag it with the class + expected
# vertex/edge attributes so we can test the S3 methods in isolation.

make_minimal_rrlm <- function(n_fn = 4L) {
  edges_df <- data.frame(
    from = c("pkg::a", "pkg::b", "pkg::c"),
    to = c("pkg::b", "pkg::c", "pkg::d"),
    weight = c(1.0, 1.0, 1.0),
    edge_type = c("CALLS", "CALLS", "CALLS"),
    stringsAsFactors = FALSE
  )
  verts_df <- data.frame(
    name = c("pkg::a", "pkg::b", "pkg::c", "pkg::d"),
    node_type = c("function", "function", "function", "function"),
    file = rep("/fake/R/pkg.R", 4L),
    line_start = c(1L, 5L, 10L, 15L),
    line_end = c(4L, 9L, 14L, 19L),
    signature = c("a(x)", "b(x)", "c(x)", "d(x)"),
    complexity = c(1L, 2L, 3L, 1L),
    pagerank = c(0.1, 0.3, 0.4, 0.2),
    stringsAsFactors = FALSE
  )

  g <- igraph::graph_from_data_frame(
    d = edges_df,
    vertices = verts_df,
    directed = TRUE
  )

  igraph::graph_attr(g, "project_name") <- "testpkg"
  igraph::graph_attr(g, "project_root") <- "/fake/testpkg"
  igraph::graph_attr(g, "project_type") <- "package"
  igraph::graph_attr(g, "r_version") <- "4.4.0"
  igraph::graph_attr(g, "build_time") <- 0.42
  igraph::graph_attr(g, "build_at") <- "2026-02-19 12:00:00"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "embed_model") <- NULL
  igraph::graph_attr(g, "cache_path") <- "/fake/.rrlmgraph/graph.rds"

  class(g) <- c("rrlm_graph", class(g))
  g
}

# ---- print.rrlm_graph -----------------------------------------------

test_that("print.rrlm_graph is a function", {
  expect_true(existsFunction("print.rrlm_graph"))
})

test_that("print.rrlm_graph returns x invisibly", {
  g <- make_minimal_rrlm()
  ret <- withVisible(print(g))
  expect_false(ret$visible)
  expect_s3_class(ret$value, "rrlm_graph")
})

test_that("print.rrlm_graph output contains project name", {
  g <- make_minimal_rrlm()
  expect_output(print(g), "testpkg")
})

test_that("print.rrlm_graph output contains node and edge counts", {
  g <- make_minimal_rrlm()
  out <- capture.output(print(g))
  combined <- paste(out, collapse = " ")
  expect_match(combined, "4") # 4 nodes
  expect_match(combined, "3") # 3 edges
})

test_that("print.rrlm_graph output contains embed method", {
  g <- make_minimal_rrlm()
  expect_output(print(g), "tfidf")
})

# ---- summary.rrlm_graph ---------------------------------------------

test_that("summary.rrlm_graph is a function", {
  expect_true(existsFunction("summary.rrlm_graph"))
})

test_that("summary.rrlm_graph returns object invisibly", {
  g <- make_minimal_rrlm()
  ret <- withVisible(summary(g))
  expect_false(ret$visible)
  expect_s3_class(ret$value, "rrlm_graph")
})

test_that("summary.rrlm_graph output contains project name", {
  g <- make_minimal_rrlm()
  expect_output(summary(g), "testpkg")
})

test_that("summary.rrlm_graph output shows node type breakdown", {
  g <- make_minimal_rrlm()
  expect_output(summary(g), "function")
})

test_that("summary.rrlm_graph output shows edge type breakdown", {
  g <- make_minimal_rrlm()
  expect_output(summary(g), "CALLS")
})

test_that("summary.rrlm_graph output mentions PageRank section", {
  g <- make_minimal_rrlm()
  out <- paste(capture.output(summary(g)), collapse = " ")
  expect_match(out, "[Pp]age[Rr]ank|pagerank", perl = TRUE)
})

test_that("summary.rrlm_graph output shows embed method", {
  g <- make_minimal_rrlm()
  expect_output(summary(g), "tfidf")
})

test_that("summary.rrlm_graph output shows build time", {
  g <- make_minimal_rrlm()
  expect_output(summary(g), "0.42")
})

# ---- summary on graph with mixed node types -------------------------

test_that("summary.rrlm_graph handles mixed node types correctly", {
  g <- make_minimal_rrlm()
  # Change one node to package type
  igraph::V(g)$node_type[1L] <- "package"
  out <- paste(capture.output(summary(g)), collapse = " ")
  expect_match(out, "package")
  expect_match(out, "function")
})

test_that("summary.rrlm_graph handles graph with no pagerank attribute", {
  g <- make_minimal_rrlm()
  g <- igraph::delete_vertex_attr(g, "pagerank")
  expect_no_error(summary(g))
})

test_that("summary.rrlm_graph handles graph with no edge_type attribute", {
  g <- make_minimal_rrlm()
  g <- igraph::delete_edge_attr(g, "edge_type")
  expect_no_error(summary(g))
})

# ---- print on empty graph -------------------------------------------

test_that("print.rrlm_graph handles zero-node graph", {
  g <- igraph::make_empty_graph(n = 0, directed = TRUE)
  igraph::graph_attr(g, "project_name") <- "empty"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  class(g) <- c("rrlm_graph", class(g))
  expect_no_error(print(g))
})

test_that("summary.rrlm_graph handles zero-node graph", {
  g <- igraph::make_empty_graph(n = 0, directed = TRUE)
  igraph::graph_attr(g, "project_name") <- "empty"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0
  class(g) <- c("rrlm_graph", class(g))
  expect_no_error(summary(g))
})

# ---- plot.rrlm_graph ------------------------------------------------

test_that("plot.rrlm_graph is a function", {
  expect_true(existsFunction("plot.rrlm_graph"))
})

test_that("plot.rrlm_graph returns a DiagrammeR htmlwidget", {
  g <- make_minimal_rrlm()
  widget <- plot(g)
  expect_s3_class(widget, "htmlwidget")
})

test_that("plot.rrlm_graph does not error with default n_hubs", {
  g <- make_minimal_rrlm()
  expect_no_error(plot(g))
})

test_that("plot.rrlm_graph respects n_hubs smaller than vcount", {
  g <- make_minimal_rrlm() # 4 nodes
  expect_no_error(plot(g, n_hubs = 2L))
})

test_that("plot.rrlm_graph handles n_hubs larger than vcount", {
  g <- make_minimal_rrlm() # 4 nodes
  expect_no_error(plot(g, n_hubs = 9999L))
})

test_that("plot.rrlm_graph handles zero-node graph without error", {
  g <- igraph::make_empty_graph(n = 0, directed = TRUE)
  igraph::graph_attr(g, "project_name") <- "empty"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  class(g) <- c("rrlm_graph", class(g))
  expect_no_warning(plot(g))
})

test_that("plot.rrlm_graph handles package and testfile node types", {
  g <- make_minimal_rrlm()
  # Add a package node (no edges needed for this test)
  g <- igraph::add_vertices(
    g,
    1,
    name = "pkgnode",
    node_type = "package",
    pagerank = 0.05
  )
  expect_no_error(plot(g, n_hubs = 3L))
})

test_that("plot.rrlm_graph accepts all layout options", {
  g <- make_minimal_rrlm()
  for (lay in c("dot", "neato", "fdp", "sfdp", "circo")) {
    expect_no_error(plot(g, layout = lay))
  }
})

# ---- S3 dispatch (UseMethod) ----------------------------------------

test_that("print dispatches to print.rrlm_graph and not print.igraph", {
  g <- make_minimal_rrlm()
  out <- capture.output(print(g))
  # print.rrlm_graph produces a single "<rrlm_graph>" line
  # print.igraph produces lines like "IGRAPH ..."
  expect_true(any(grepl("rrlm_graph", out, fixed = TRUE)))
})

test_that("summary dispatches to summary.rrlm_graph", {
  g <- make_minimal_rrlm()
  # summary.rrlm_graph emits cli_h1 heading containing project name
  out <- paste(capture.output(summary(g)), collapse = " ")
  # Should contain our project name, not raw igraph summary
  expect_match(out, "testpkg")
})
