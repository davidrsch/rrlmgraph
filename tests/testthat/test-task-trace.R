# tests/testthat/test-task-trace.R
# Unit tests for log_task_trace(), update_task_weights(),
# update_task_polarity()
# rrlmgraph issue #18 acceptance criteria.

skip_if_not_installed("igraph")

# ---- fixture helpers ------------------------------------------------

make_tt_graph <- function(project_root = NULL) {
  verts <- data.frame(
    name = c("pkg::load_data", "pkg::clean", "pkg::model"),
    node_type = "function",
    pagerank = c(0.5, 0.3, 0.2),
    task_trace_weight = c(0.5, 0.5, 0.5),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("pkg::load_data", "pkg::clean"),
    to = c("pkg::clean", "pkg::model"),
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
  class(g) <- c("rrlm_graph", class(g))
  g
}

# ---- log_task_trace -------------------------------------------------

test_that("log_task_trace creates JSONL file", {
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  path <- log_task_trace(
    "How does load_data work?",
    c("pkg::load_data", "pkg::clean"),
    g
  )

  expect_true(file.exists(path))
  lines <- readLines(path)
  expect_length(lines, 1L)
})

test_that("log_task_trace appends multiple entries", {
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  log_task_trace("query 1", c("pkg::load_data"), g, polarity = 0.5)
  log_task_trace("query 2", c("pkg::model"), g, polarity = -0.5)

  path <- file.path(tmp, ".rrlmgraph", "task_trace.jsonl")
  lines <- readLines(path)
  expect_length(lines, 2L)
})

test_that("log_task_trace JSONL entries are valid JSON", {
  skip_if_not_installed("jsonlite")
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  log_task_trace("test query", c("pkg::load_data"), g, polarity = 0.8)

  path <- file.path(tmp, ".rrlmgraph", "task_trace.jsonl")
  line <- readLines(path, n = 1L)
  entry <- jsonlite::fromJSON(line)

  expect_equal(entry$query, "test query")
  expect_equal(entry$polarity, 0.8)
  expect_true("nodes" %in% names(entry))
  expect_true("timestamp" %in% names(entry))
})

test_that("log_task_trace rejects bad polarity", {
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)
  expect_error(
    log_task_trace("q", "pkg::load_data", g, polarity = 2),
    "polarity"
  )
})

test_that("log_task_trace returns path invisibly", {
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)
  res <- withVisible(log_task_trace("q", "pkg::load_data", g))
  expect_false(res$visible)
  expect_true(file.exists(res$value))
})

# ---- update_task_weights (EMA fallback) -----------------------------

test_that("update_task_weights initialises task_trace_weight if absent", {
  g_bare <- igraph::make_ring(3L)
  igraph::V(g_bare)$name <- c("a", "b", "c")
  g_out <- update_task_weights(g_bare, useful_nodes = "a")
  expect_false(is.null(igraph::V(g_out)$task_trace_weight))
})

test_that("update_task_weights boosts useful nodes (EMA path)", {
  g <- make_tt_graph()
  g_out <- update_task_weights(
    g,
    useful_nodes = c("pkg::load_data"),
    alpha = 0.3,
    decay = 0.99
  )

  w_before <- igraph::V(g)$task_trace_weight
  w_after <- igraph::V(g_out)$task_trace_weight

  load_idx <- match("pkg::load_data", igraph::V(g_out)$name)
  other_idx <- setdiff(seq_along(w_after), load_idx)

  expect_gt(w_after[load_idx], w_before[load_idx])
  expect_lt(w_after[other_idx[1L]], w_before[other_idx[1L]])
})

test_that("update_task_weights weights stay in [0.1, 1.0]", {
  skip_if_not_installed("jsonlite")
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  # Write several JSONL entries
  for (i in seq_len(5L)) {
    log_task_trace(
      paste("q", i),
      c("pkg::load_data", "pkg::clean"),
      g,
      polarity = 0.5
    )
  }

  g_out <- update_task_weights(g)
  w <- igraph::V(g_out)$task_trace_weight

  expect_true(all(w >= 0.1))
  expect_true(all(w <= 1.0))
})

test_that("update_task_weights accepts NULL useful_nodes", {
  g <- make_tt_graph()
  expect_no_error(update_task_weights(g, useful_nodes = NULL))
})

# ---- update_task_polarity -------------------------------------------

test_that("update_task_polarity rewrites polarity in recent entries", {
  skip_if_not_installed("jsonlite")
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  log_task_trace("query 1", c("pkg::load_data"), g, polarity = 0)
  log_task_trace("query 2", c("pkg::load_data"), g, polarity = 0)

  ctx <- structure(
    list(
      nodes = c("pkg::load_data"),
      context_string = "test context",
      tokens_used = 1L,
      budget_tokens = 100L,
      seed_node = "pkg::load_data",
      relevance_scores = c("pkg::load_data" = 1.0)
    ),
    class = c("rrlm_context", "list")
  )
  update_task_polarity(
    g,
    context = ctx,
    polarity = 1,
    n_recent = 2L
  )

  path <- file.path(tmp, ".rrlmgraph", "task_trace.jsonl")
  lines <- readLines(path)
  entries <- lapply(lines, jsonlite::fromJSON)
  polarities <- vapply(entries, `[[`, numeric(1), "polarity")
  expect_true(all(polarities == 1))
})

test_that("update_task_polarity only rewrites n_recent entries", {
  skip_if_not_installed("jsonlite")
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  for (i in seq_len(4L)) {
    log_task_trace(paste("q", i), c("pkg::load_data"), g, polarity = 0)
  }

  ctx <- structure(
    list(
      nodes = c("pkg::load_data"),
      context_string = "test context",
      tokens_used = 1L,
      budget_tokens = 100L,
      seed_node = "pkg::load_data",
      relevance_scores = c("pkg::load_data" = 1.0)
    ),
    class = c("rrlm_context", "list")
  )
  update_task_polarity(
    g,
    context = ctx,
    polarity = 0.9,
    n_recent = 2L
  )

  path <- file.path(tmp, ".rrlmgraph", "task_trace.jsonl")
  lines <- readLines(path)
  entries <- lapply(lines, jsonlite::fromJSON)
  pols <- vapply(entries, `[[`, numeric(1), "polarity")

  # First 2 entries unchanged, last 2 should be updated
  expect_equal(pols[1L], 0)
  expect_equal(pols[2L], 0)
  expect_equal(pols[3L], 0.9)
  expect_equal(pols[4L], 0.9)
})

test_that("update_task_polarity returns invisible trace file path", {
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)
  log_task_trace("q", c("pkg::load_data"), g)
  ctx <- structure(
    list(
      nodes = c("pkg::load_data"),
      context_string = "test context",
      tokens_used = 1L,
      budget_tokens = 100L,
      seed_node = "pkg::load_data",
      relevance_scores = c("pkg::load_data" = 1.0)
    ),
    class = c("rrlm_context", "list")
  )
  res <- withVisible(
    update_task_polarity(g, context = ctx, polarity = 0.5)
  )
  expect_false(res$visible)
})
