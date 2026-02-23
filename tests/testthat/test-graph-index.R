# tests/testthat/test-graph-index.R
# Unit tests for update_graph_incremental()
# rrlmgraph issue #17 acceptance criteria.

skip_if_not_installed("igraph")

# ---- fixture helpers ------------------------------------------------

# A minimal rrlm_graph with nodes attributed to specific R files
make_index_graph <- function(project_root = NULL) {
  verts <- data.frame(
    name = c("pkg::alpha", "pkg::beta", "pkg::gamma"),
    node_type = "function",
    pagerank = c(0.5, 0.3, 0.2),
    task_trace_weight = c(0.5, 0.5, 0.5),
    file = c("R/alpha.R", "R/beta.R", "R/beta.R"),
    pkg = "pkg",
    doc = c("Alpha function", "Beta function", "Gamma function"),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("pkg::alpha", "pkg::beta"),
    to = c("pkg::beta", "pkg::gamma"),
    weight = 1.0,
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  if (!is.null(project_root)) {
    igraph::graph_attr(g, "project_root") <- project_root
  }
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  class(g) <- c("rrlm_graph", class(g))
  g
}

# ---- argument validation --------------------------------------------

test_that("update_graph_incremental rejects non-igraph input", {
  expect_error(
    update_graph_incremental(list(), changed_files = "R/foo.R"),
    "igraph"
  )
})

test_that("update_graph_incremental rejects empty changed_files", {
  g <- make_index_graph()
  expect_error(
    update_graph_incremental(g, changed_files = character(0)),
    "changed_files"
  )
})

# ---- node removal ---------------------------------------------------

test_that("stale nodes from changed_files are removed", {
  tmp <- withr::local_tempdir()
  # Write a minimal R file in a temp sub-directory
  r_dir <- file.path(tmp, "R")
  dir.create(r_dir)
  # Don't create the file so it looks like a deletion
  target <- file.path(r_dir, "beta.R")

  g <- make_index_graph(project_root = tmp)
  # Replace "R/beta.R" with absolute path in graph vertex attrs
  igraph::V(g)$file <- ifelse(
    igraph::V(g)$file == "R/beta.R",
    target,
    file.path(tmp, igraph::V(g)$file)
  )
  igraph::V(g)$file[igraph::V(g)$file == file.path(tmp, "R/alpha.R")] <-
    file.path(tmp, "R", "alpha.R")

  n_before <- igraph::vcount(g)
  g_out <- update_graph_incremental(
    g,
    changed_files = target,
    embed_method = "tfidf"
  )

  expect_lt(igraph::vcount(g_out), n_before)
  remaining <- igraph::V(g_out)$name
  expect_false(any(remaining %in% c("pkg::beta", "pkg::gamma")))
  expect_true("pkg::alpha" %in% remaining)
})

# ---- PageRank recomputed --------------------------------------------

test_that("PageRank is recomputed after update", {
  tmp <- withr::local_tempdir()
  g <- make_index_graph(project_root = tmp)
  # Use a non-existent file (treated as deletion)
  g_out <- update_graph_incremental(
    g,
    changed_files = "nonexistent.R",
    embed_method = "tfidf"
  )

  pr <- igraph::V(g_out)$pagerank
  expect_false(is.null(pr))
  expect_true(all(!is.na(pr)))
  expect_equal(sum(pr), 1, tolerance = 0.01)
})

# ---- empty changed_files with no match -------------------------------

test_that("passing a non-matching changed_file returns graph unchanged in nodes", {
  g <- make_index_graph()
  n_bef <- igraph::vcount(g)
  g_out <- update_graph_incremental(
    g,
    changed_files = "does_not_exist.R",
    embed_method = "tfidf"
  )
  expect_equal(igraph::vcount(g_out), n_bef)
})

# ---- verbose flag ---------------------------------------------------

test_that("verbose = TRUE emits messages without error", {
  g <- make_index_graph()
  expect_no_error(
    suppressMessages(
      update_graph_incremental(
        g,
        changed_files = "no_match.R",
        embed_method = "tfidf",
        verbose = TRUE
      )
    )
  )
})

# ---- return class ---------------------------------------------------

test_that("output retains rrlm_graph class", {
  g <- make_index_graph()
  g_out <- update_graph_incremental(
    g,
    changed_files = "no_match.R",
    embed_method = "tfidf"
  )
  expect_s3_class(g_out, "rrlm_graph")
})

# ---- new node addition (covers the extant-file → merge path) --------

test_that("update_graph_incremental adds nodes from a new R file", {
  tmp <- withr::local_tempdir()

  # Create an R file with a function definition
  new_file <- file.path(tmp, "new_fn.R")
  writeLines(
    c("new_function <- function(x) x * 2"),
    new_file
  )

  g <- make_index_graph(project_root = tmp)
  n_before <- igraph::vcount(g)

  g_out <- suppressWarnings(
    update_graph_incremental(
      g,
      changed_files = new_file,
      embed_method = "tfidf"
    )
  )

  # New node(s) should be added
  expect_gt(igraph::vcount(g_out), n_before)
  expect_true(any(grepl("new_function", igraph::V(g_out)$name)))
})

test_that("update_graph_incremental verbose=TRUE shows messages with new file", {
  tmp <- withr::local_tempdir()

  new_file <- file.path(tmp, "verbose_fn.R")
  writeLines(
    c("verbose_function <- function(a, b) a + b"),
    new_file
  )

  g <- make_index_graph(project_root = tmp)

  expect_no_error(
    suppressMessages(
      suppressWarnings(
        update_graph_incremental(
          g,
          changed_files = new_file,
          embed_method = "tfidf",
          verbose = TRUE
        )
      )
    )
  )
})
