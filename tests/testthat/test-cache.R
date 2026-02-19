# tests/testthat/test-cache.R
# Unit tests for save_graph_cache(), load_graph_cache(), is_cache_stale()
# Issue #14 acceptance criteria.

skip_if_not_installed("igraph")

# ---- fixture helpers ------------------------------------------------

make_cache_graph <- function(project_root = NULL) {
  verts <- data.frame(
    name = c("pkg::alpha", "pkg::beta", "pkg::gamma"),
    node_type = "function",
    pagerank = c(0.5, 0.3, 0.2),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("pkg::alpha", "pkg::beta"),
    to = c("pkg::beta", "pkg::gamma"),
    weight = 1.0,
    edge_type = "CALLS",
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "cachepkg"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  if (!is.null(project_root)) {
    igraph::graph_attr(g, "project_root") <- project_root
  }
  class(g) <- c("rrlm_graph", class(g))
  g
}

# ---- save_graph_cache -----------------------------------------------

test_that("save_graph_cache creates expected files", {
  tmp <- withr::local_tempdir()
  g <- make_cache_graph()

  save_graph_cache(g, cache_dir = tmp)

  expect_true(file.exists(file.path(tmp, "graph.rds")))
  expect_true(file.exists(file.path(tmp, "config.yml")))
  expect_true(file.exists(file.path(tmp, ".gitignore")))
})

test_that("save_graph_cache config.yml contains package_version", {
  tmp <- withr::local_tempdir()
  g <- make_cache_graph()
  save_graph_cache(g, cache_dir = tmp)

  yml <- readLines(file.path(tmp, "config.yml"), warn = FALSE)
  expect_true(any(grepl("^package_version:", yml)))
})

test_that("save_graph_cache config.yml contains node_count and edge_count", {
  tmp <- withr::local_tempdir()
  g <- make_cache_graph()
  save_graph_cache(g, cache_dir = tmp)

  yml <- readLines(file.path(tmp, "config.yml"), warn = FALSE)
  expect_true(any(grepl("^node_count:", yml)))
  expect_true(any(grepl("^edge_count:", yml)))
})

test_that("save_graph_cache .gitignore ignores *.rds but tracks config.yml", {
  tmp <- withr::local_tempdir()
  g <- make_cache_graph()
  save_graph_cache(g, cache_dir = tmp)

  gi <- readLines(file.path(tmp, ".gitignore"), warn = FALSE)
  expect_true(any(grepl("\\*\\.rds", gi)))
  expect_true(any(grepl("!config\\.yml", gi)))
  expect_true(any(grepl("!task_trace\\.jsonl", gi)))
})

test_that("save_graph_cache returns cache_dir invisibly", {
  tmp <- withr::local_tempdir()
  g <- make_cache_graph()

  ret <- withVisible(save_graph_cache(g, cache_dir = tmp))
  expect_false(ret$visible)
  expect_equal(ret$value, tmp)
})

test_that("save_graph_cache uses project_root when cache_dir is NULL", {
  tmp <- withr::local_tempdir()
  g <- make_cache_graph(project_root = tmp)

  expect_no_error(save_graph_cache(g))
  expected_dir <- file.path(tmp, ".rrlmgraph")
  expect_true(file.exists(file.path(expected_dir, "graph.rds")))
})

# ---- load_graph_cache -----------------------------------------------

test_that("load_graph_cache round-trip returns isomorphic graph", {
  tmp <- withr::local_tempdir()
  g <- make_cache_graph(project_root = tmp)
  save_graph_cache(g, cache_dir = file.path(tmp, ".rrlmgraph"))

  g2 <- load_graph_cache(tmp)

  expect_s3_class(g2, "rrlm_graph")
  expect_equal(igraph::vcount(g2), igraph::vcount(g))
  expect_equal(igraph::ecount(g2), igraph::ecount(g))
  expect_true(igraph::isomorphic(g, g2))
})

test_that("load_graph_cache returns NULL on missing cache", {
  tmp <- withr::local_tempdir()

  result <- suppressWarnings(load_graph_cache(tmp))
  expect_null(result)
})

test_that("load_graph_cache warns (not errors) on missing cache", {
  tmp <- withr::local_tempdir()

  expect_warning(load_graph_cache(tmp), regexp = "No cache")
})

test_that("load_graph_cache preserves graph attributes", {
  tmp <- withr::local_tempdir()
  g <- make_cache_graph(project_root = tmp)
  save_graph_cache(g, cache_dir = file.path(tmp, ".rrlmgraph"))

  g2 <- load_graph_cache(tmp)
  expect_equal(
    igraph::graph_attr(g2, "project_name"),
    igraph::graph_attr(g, "project_name")
  )
})

# ---- is_cache_stale -------------------------------------------------

test_that("is_cache_stale returns TRUE when no cache exists", {
  tmp <- withr::local_tempdir()
  expect_true(is_cache_stale(tmp))
})

test_that("is_cache_stale returns FALSE immediately after save", {
  tmp <- withr::local_tempdir()
  g <- make_cache_graph(project_root = tmp)
  save_graph_cache(g, cache_dir = file.path(tmp, ".rrlmgraph"))

  # No R files in tmp, so nothing newer
  expect_false(is_cache_stale(tmp))
})

test_that("is_cache_stale returns TRUE when an R file is newer than cache", {
  tmp <- withr::local_tempdir()
  g <- make_cache_graph(project_root = tmp)
  cache <- file.path(tmp, ".rrlmgraph")
  save_graph_cache(g, cache_dir = cache)

  # Create an R file that appears newer than the cache
  r_file <- file.path(tmp, "new_func.R")
  writeLines("new_func <- function() {}", r_file)
  # Manually bump the mtime one second into the future
  Sys.setFileTime(r_file, Sys.time() + 2)

  expect_true(is_cache_stale(tmp))
})

# ---- non-igraph input errors ----------------------------------------

test_that("save_graph_cache errors on non-igraph input", {
  expect_error(save_graph_cache(list()), regexp = "igraph")
})

test_that("load_graph_cache errors on corrupt RDS", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, ".rrlmgraph"), recursive = TRUE)
  writeLines("not an rds file", file.path(tmp, ".rrlmgraph", "graph.rds"))

  expect_error(load_graph_cache(tmp))
})
