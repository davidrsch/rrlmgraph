# tests/testthat/test-rrlm-graph-extra.R
# Branch-coverage tests for rrlm_graph.R S3 methods that do NOT require
# the mini_ds_project fixture (available without rrlmgraph-bench).

skip_if_not_installed("igraph")

# ---- fixture helper -------------------------------------------------

.make_minimal_rrlm_graph_x <- function(
  node_names = c("pkg::a", "pkg::b"),
  add_edges = TRUE,
  node_type = "function"
) {
  verts <- data.frame(
    name = node_names,
    node_type = node_type,
    pagerank = seq_len(length(node_names)) / length(node_names),
    stringsAsFactors = FALSE
  )
  if (add_edges && length(node_names) >= 2L) {
    edges <- data.frame(
      from = node_names[[1L]],
      to = node_names[[2L]],
      weight = 1.0,
      edge_type = "CALLS",
      stringsAsFactors = FALSE
    )
  } else {
    edges <- data.frame(
      from = character(0),
      to = character(0),
      weight = numeric(0),
      edge_type = character(0),
      stringsAsFactors = FALSE
    )
  }
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "testpkg"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0.1
  igraph::graph_attr(g, "build_at") <- "2024-01-01"
  class(g) <- c("rrlm_graph", class(g))
  g
}

# ---- summary.rrlm_graph branches ------------------------------------

test_that("summary.rrlm_graph handles graph with 0 nodes", {
  g_empty <- igraph::make_empty_graph(0, directed = TRUE)
  class(g_empty) <- c("rrlm_graph", class(g_empty))
  igraph::graph_attr(g_empty, "project_name") <- "empty_pkg"
  igraph::graph_attr(g_empty, "embed_method") <- "tfidf"
  igraph::graph_attr(g_empty, "build_time") <- 0
  igraph::graph_attr(g_empty, "build_at") <- "2024-01-01"
  expect_output(summary(g_empty), "rrlm_graph")
})

test_that("summary.rrlm_graph handles graph with null node_type attribute", {
  g <- igraph::make_ring(3L, directed = TRUE)
  igraph::V(g)$name <- c("a", "b", "c")
  class(g) <- c("rrlm_graph", class(g))
  igraph::graph_attr(g, "project_name") <- "notype_pkg"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0
  igraph::graph_attr(g, "build_at") <- "2024-01-01"
  expect_output(summary(g), "rrlm_graph")
})

test_that("summary.rrlm_graph handles graph with null edge_type attribute", {
  g <- .make_minimal_rrlm_graph_x()
  g2 <- igraph::delete_edge_attr(g, "edge_type") # NULL-safe removal
  expect_output(summary(g2), "rrlm_graph")
})

test_that("summary.rrlm_graph handles missing build_time attribute", {
  g <- .make_minimal_rrlm_graph_x()
  igraph::graph_attr(g, "build_time") <- NA_real_
  expect_output(summary(g), "rrlm_graph")
})

test_that("print.rrlm_graph outputs one line on minimal graph", {
  g <- .make_minimal_rrlm_graph_x()
  expect_output(print(g), "rrlm_graph")
})

# ---- plot.rrlm_graph branches ----------------------------------------

test_that("plot.rrlm_graph returns invisibly and messages on empty graph", {
  skip_if_not_installed("DiagrammeR")
  g <- igraph::make_empty_graph(0, directed = TRUE)
  class(g) <- c("rrlm_graph", class(g))
  igraph::graph_attr(g, "project_name") <- "empty"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0
  igraph::graph_attr(g, "build_at") <- "2024-01-01"
  expect_message(
    res <- withVisible(plot(g)),
    regexp = "no nodes"
  )
  expect_false(res$visible)
})

test_that("plot.rrlm_graph messages when sub-graph has only 1 node", {
  skip_if_not_installed("DiagrammeR")
  g <- .make_minimal_rrlm_graph_x(
    node_names = "pkg::lonely",
    add_edges = FALSE
  )
  expect_message(
    plot(g, n_hubs = 1L),
    regexp = "Sub-graph has only"
  )
})

test_that("plot.rrlm_graph handles null node_type (pagerank present)", {
  skip_if_not_installed("DiagrammeR")
  # node_type absent → `is.null(nt_all)` branch in plot.rrlm_graph;
  # pagerank IS set to avoid crash in .rrlmgraph_to_dot()
  g <- igraph::make_ring(3L, directed = TRUE)
  igraph::V(g)$name <- c("pkg::x", "pkg::y", "pkg::z")
  igraph::V(g)$pagerank <- c(0.5, 0.3, 0.2)
  class(g) <- c("rrlm_graph", class(g))
  igraph::graph_attr(g, "project_name") <- "noattr"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0
  igraph::graph_attr(g, "build_at") <- "2024-01-01"
  expect_no_error(suppressMessages(plot(g, n_hubs = 2L)))
})

test_that("plot.rrlm_graph errors on unsupported file extension", {
  skip_if_not_installed("DiagrammeR")
  skip_if_not_installed("htmlwidgets")
  g <- .make_minimal_rrlm_graph_x()
  tmp <- tempfile(fileext = ".xyz")
  on.exit(unlink(tmp), add = TRUE)
  expect_error(plot(g, file = tmp), regexp = "Unsupported")
})
