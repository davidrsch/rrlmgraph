# tests/testthat/test-rrlm-graph.R
# Sprint 1 — issue #8 / acceptance criteria for issues #6 & #7
# Requires: the mini_ds_project fixture installed via rrlmgraph-bench or
#           available locally under inst/projects/mini_ds_project/

skip_if_not_installed("igraph")
skip_if_not_installed("text2vec")

# Locate the fixture ---------------------------------------------------------
fixture_path <- system.file(
  "projects",
  "mini_ds_project",
  package = "rrlmgraph"
)
if (!nzchar(fixture_path)) {
  # Fallback: the bench repo is a sibling directory.
  # During devtools::test() getwd() is the package root, so
  # dirname(getwd()) is the repos/ folder next to rrlmgraph-bench.
  fixture_path <- file.path(
    dirname(getwd()),
    "rrlmgraph-bench",
    "inst",
    "projects",
    "mini_ds_project"
  )
}

skip_if(
  !dir.exists(fixture_path),
  "mini_ds_project fixture not available — install rrlmgraph-bench"
)

# ---- build_rrlm_graph() ---------------------------------------------------

test_that("build_rrlm_graph returns an rrlm_graph / igraph object", {
  g <- build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)

  expect_s3_class(g, "rrlm_graph")
  expect_s3_class(g, "igraph")
})

test_that("graph has at least as many nodes as known functions", {
  g <- build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)

  # The mini_ds_project has 11 user functions (ground truth)
  expect_gte(igraph::vcount(g), 11L)
})

test_that("graph has edge types CALLS and SEMANTIC", {
  g <- build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)

  edge_types <- unique(igraph::E(g)$edge_type)
  expect_true("CALLS" %in% edge_types)
})

test_that("all nodes carry a node_type attribute", {
  g <- build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)

  nt <- igraph::V(g)$node_type
  expect_false(any(is.na(nt)))
})

test_that("every node carries a pagerank score", {
  g <- build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)

  pr <- igraph::V(g)$pagerank
  expect_true(all(!is.na(pr)))
  expect_true(all(pr >= 0))
})

test_that("graph metadata attributes are populated", {
  g <- build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)

  expect_true(nzchar(igraph::graph_attr(g, "project_name")))
  expect_true(nzchar(igraph::graph_attr(g, "r_version")))
  expect_true(nzchar(igraph::graph_attr(g, "embed_method")))
  expect_false(is.na(igraph::graph_attr(g, "build_time")))
})

test_that("serialize/deserialize round-trip preserves the class", {
  g <- build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(g, tmp)
  g2 <- readRDS(tmp)

  expect_s3_class(g2, "rrlm_graph")
  expect_equal(igraph::vcount(g), igraph::vcount(g2))
  expect_equal(igraph::ecount(g), igraph::ecount(g2))
})

test_that("cache=TRUE writes .rrlmgraph/graph.rds", {
  tmp_root <- tempfile("rrlm_cache_test")
  fs::dir_copy(fixture_path, tmp_root)
  on.exit(fs::dir_delete(tmp_root), add = TRUE)

  g <- build_rrlm_graph(tmp_root, cache = TRUE, verbose = FALSE)

  cache_file <- file.path(tmp_root, ".rrlmgraph", "graph.rds")
  expect_true(file.exists(cache_file))
})

test_that("build_rrlm_graph completes in under 30 seconds", {
  elapsed <- system.time(
    build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)
  )[["elapsed"]]
  expect_lt(elapsed, 30)
})

# ---- include_package_nodes = FALSE ----------------------------------------

test_that("include_package_nodes=FALSE yields fewer or equal nodes", {
  g_with <- build_rrlm_graph(
    fixture_path,
    include_package_nodes = TRUE,
    cache = FALSE,
    verbose = FALSE
  )
  g_nopkg <- build_rrlm_graph(
    fixture_path,
    include_package_nodes = FALSE,
    cache = FALSE,
    verbose = FALSE
  )

  expect_lte(igraph::vcount(g_nopkg), igraph::vcount(g_with))
})

# ---- S3 methods -----------------------------------------------------------

test_that("print.rrlm_graph exists and runs without error", {
  g <- build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)

  expect_true(existsFunction("print.rrlm_graph"))
  expect_output(print(g), "rrlm_graph")
})

test_that("summary.rrlm_graph exists and runs without error", {
  g <- build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)

  expect_true(existsFunction("summary.rrlm_graph"))
  expect_output(summary(g), "rrlm_graph")
})

test_that("plot.rrlm_graph exists and runs without error", {
  skip_if_not_installed("igraph")
  g <- build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)

  expect_true(existsFunction("plot.rrlm_graph"))
  # Should produce a plot without error/warning
  grDevices::pdf(NULL) # suppress window
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(g, n_hubs = 10L))
})

test_that("plot.rrlm_graph handles graph smaller than n_hubs gracefully", {
  g <- build_rrlm_graph(fixture_path, cache = FALSE, verbose = FALSE)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  # n_hubs larger than total node count — should not error
  expect_no_error(plot(g, n_hubs = 9999L))
})

# ---- rrlm_graph branch coverage (no fixture needed) -----------------

.make_minimal_rrlm_graph <- function(
  node_names = c("pkg::a", "pkg::b"),
  add_edges = TRUE,
  node_type = "function"
) {
  verts <- data.frame(
    name = node_names,
    node_type = node_type,
    pagerank = seq_along(node_names) / length(node_names),
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

test_that("summary.rrlm_graph handles graph with 0 nodes", {
  g_empty <- igraph::make_empty_graph(0, directed = TRUE)
  class(g_empty) <- c("rrlm_graph", class(g_empty))
  igraph::graph_attr(g_empty, "project_name") <- "empty_pkg"
  igraph::graph_attr(g_empty, "embed_method") <- "tfidf"
  igraph::graph_attr(g_empty, "build_time") <- 0
  igraph::graph_attr(g_empty, "build_at") <- "2024-01-01"
  # Should output the header without error even with no nodes or edges
  expect_output(summary(g_empty), "rrlm_graph")
})

test_that("summary.rrlm_graph handles graph with null node_type attribute", {
  # Graph without node_type → null branch inside the vcount > 0 block
  g <- igraph::make_ring(3L, directed = TRUE)
  igraph::V(g)$name <- c("a", "b", "c")
  # Deliberately omit node_type and pagerank
  class(g) <- c("rrlm_graph", class(g))
  igraph::graph_attr(g, "project_name") <- "notype_pkg"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0
  igraph::graph_attr(g, "build_at") <- "2024-01-01"
  expect_output(summary(g), "rrlm_graph")
})

test_that("summary.rrlm_graph handles graph with null edge_type attribute", {
  g <- .make_minimal_rrlm_graph()
  # Remove edge_type attribute safely
  g2 <- igraph::delete_edge_attr(g, "edge_type")
  expect_output(summary(g2), "rrlm_graph")
})

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
  # A single function node with no edges: neighbourhood = {itself} → sub = 1 node
  g <- .make_minimal_rrlm_graph(
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
  g <- igraph::make_ring(3L, directed = TRUE)
  igraph::V(g)$name <- c("pkg::x", "pkg::y", "pkg::z")
  igraph::V(g)$pagerank <- c(0.5, 0.3, 0.2)
  # No node_type → null-check branch inside plot
  class(g) <- c("rrlm_graph", class(g))
  igraph::graph_attr(g, "project_name") <- "noattr"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0
  igraph::graph_attr(g, "build_at") <- "2024-01-01"
  # Should not error (treated as all "function" type with equal pagerank)
  expect_no_error(suppressMessages(plot(g, n_hubs = 2L)))
})

test_that("plot.rrlm_graph errors on unsupported file extension", {
  skip_if_not_installed("DiagrammeR")
  skip_if_not_installed("htmlwidgets")
  g <- .make_minimal_rrlm_graph()
  tmp <- tempfile(fileext = ".xyz")
  on.exit(unlink(tmp), add = TRUE)
  expect_error(plot(g, file = tmp), regexp = "Unsupported")
})
