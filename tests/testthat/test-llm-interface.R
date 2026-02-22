# tests/testthat/test-llm-interface.R
# Unit tests for chat_with_context() and related helpers.
# Issue #13 acceptance criteria.

skip_if_not_installed("igraph")

# ---- fixture helpers ------------------------------------------------

make_llm_graph <- function() {
  verts <- data.frame(
    name = c("pkg::fit_model", "pkg::predict_model"),
    node_type = c("function", "function"),
    pagerank = c(0.6, 0.4),
    body = c(
      "function(data) { lm(y ~ x, data = data) }",
      "function(model, data) { predict(model, data) }"
    ),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = "pkg::fit_model",
    to = "pkg::predict_model",
    weight = 1.0,
    edge_type = "CALLS",
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "pkgllm"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  class(g) <- c("rrlm_graph", class(g))
  g
}

# ---- input validation -----------------------------------------------

test_that("non-igraph graph raises error", {
  expect_error(
    chat_with_context(list(), "hello"),
    regexp = "igraph"
  )
})

test_that("non-character message raises error", {
  g <- make_llm_graph()
  expect_error(
    chat_with_context(g, 42L),
    regexp = "message"
  )
})

test_that("message length > 1 raises error", {
  g <- make_llm_graph()
  expect_error(
    chat_with_context(g, c("a", "b")),
    regexp = "message"
  )
})

# ---- .build_system_prompt -------------------------------------------

test_that(".build_system_prompt with empty context returns grounding only", {
  sp <- rrlmgraph:::.build_system_prompt("")

  expect_type(sp, "character")
  expect_length(sp, 1L)
  expect_match(sp, "RULES", fixed = TRUE)
  expect_false(grepl("BEGIN CODE CONTEXT", sp, fixed = TRUE))
})

test_that(".build_system_prompt embeds non-empty context", {
  sp <- rrlmgraph:::.build_system_prompt("function foo() { 1 }")

  expect_match(sp, "BEGIN CODE CONTEXT", fixed = TRUE)
  expect_match(sp, "END CODE CONTEXT", fixed = TRUE)
  expect_match(sp, "function foo", fixed = TRUE)
})

test_that(".build_system_prompt grounding instructs context-only answers", {
  sp <- rrlmgraph:::.build_system_prompt("ctx text")

  expect_match(sp, "ONLY on the code context", fixed = TRUE)
})

# ---- prompt construction via chat_with_context ----------------------

test_that("chat_with_context returns character(1) with mocked backend", {
  g <- make_llm_graph()

  # Mock both paths so the test passes regardless of whether ellmer is installed
  with_mocked_bindings(
    .llm_via_ellmer = function(sp, msg, provider, model, ...) "mocked response",
    .llm_via_httr2 = function(sp, msg, model) "mocked response",
    .package = "rrlmgraph",
    {
      result <- chat_with_context(g, "What does fit_model do?")
    }
  )

  expect_type(result, "character")
  expect_length(result, 1L)
  expect_equal(result, "mocked response")
})

test_that("chat_with_context returns error string when LLM fails", {
  g <- make_llm_graph()

  with_mocked_bindings(
    .llm_via_ellmer = function(sp, msg, provider, model, ...) {
      stop("connection timeout")
    },
    .llm_via_httr2 = function(sp, msg, model) stop("connection timeout"),
    .package = "rrlmgraph",
    {
      result <- suppressWarnings(
        chat_with_context(g, "how does predict work?")
      )
    }
  )

  expect_type(result, "character")
  expect_match(result, "\\[rrlmgraph error\\]")
})

test_that("chat_with_context dispatches correct provider to .llm_via_ellmer", {
  g <- make_llm_graph()
  captured <- list()

  with_mocked_bindings(
    .llm_via_ellmer = function(sp, msg, provider, model, ...) {
      captured$provider <<- provider
      captured$model <<- model
      "ok"
    },
    .llm_via_httr2 = function(sp, msg, model) "ok",
    .package = "rrlmgraph",
    {
      chat_with_context(g, "test", provider = "github", model = "gpt-4o")
    }
  )

  # Only check when ellmer path was actually taken
  if (!is.null(captured$provider)) {
    expect_equal(captured$provider, "github")
    expect_equal(captured$model, "gpt-4o")
  }
})

test_that("chat_with_context rejects invalid provider", {
  g <- make_llm_graph()
  expect_error(
    chat_with_context(g, "test", provider = "invalid_provider")
  )
})

# ---- task logging ---------------------------------------------------

test_that(".log_task_completion writes nothing when project_root absent", {
  g <- make_llm_graph() # no project_root attribute

  # Should not error
  expect_no_error(
    rrlmgraph:::.log_task_completion(
      graph = g,
      query = "test",
      nodes = c("pkg::fit_model"),
      response = "ok"
    )
  )
})

test_that(".log_task_completion writes JSONL when project_root is set", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()

  g <- make_llm_graph()
  igraph::graph_attr(g, "project_root") <- tmp

  rrlmgraph:::.log_task_completion(
    graph = g,
    query = "train model",
    nodes = c("pkg::fit_model"),
    response = "The fit_model function..."
  )

  trace_file <- file.path(tmp, ".rrlmgraph", "task_trace.jsonl")
  expect_true(file.exists(trace_file))

  lines <- readLines(trace_file, warn = FALSE)
  expect_gte(length(lines), 1L)

  entry <- jsonlite::fromJSON(lines[[1L]])
  expect_equal(entry$query, "train model")
  expect_true("pkg::fit_model" %in% entry$nodes)
})
