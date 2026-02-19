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
  # Fallback: look for the bench repo alongside this checkout
  fixture_path <- file.path(
    dirname(dirname(dirname(dirname(getwd())))),
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
