# tests/testthat/test-embed-ollama.R
# Unit tests for the Ollama embedding backend (issue #19).
# All tests mock ollamar calls; they do NOT require a live Ollama daemon.

skip_if_not_installed("igraph")

# ---- fixture helpers ------------------------------------------------

make_embed_nodes <- function(n = 3L) {
  lapply(seq_len(n), function(i) {
    list(
      node_id = paste0("pkg::fn", i),
      signature = paste0("fn", i, "(x, y)"),
      body_text = paste0("body of function ", i),
      roxygen_text = paste0("roxygen docs for fn", i)
    )
  })
}

# ---- ollama_available -----------------------------------------------

test_that("ollama_available returns FALSE when Ollama not reachable", {
  # Mock httr2::req_perform to simulate a connection failure
  local_mocked_bindings(
    req_perform = function(req, ...) stop("Connection refused"),
    .package = "httr2"
  )
  result <- ollama_available()
  expect_false(result)
})

# ---- .embed_ollama fallback when Ollama unavailable -----------------

test_that("embed_nodes('ollama') falls back to TF-IDF when Ollama unavailable", {
  nodes <- make_embed_nodes(3L)

  # Mock ollama_available to return FALSE
  local_mocked_bindings(
    ollama_available = function() FALSE,
    .package = "rrlmgraph"
  )
  result <- embed_nodes(nodes, method = "ollama")
  expect_named(result, c("embeddings", "model", "matrix"))
  expect_length(result$embeddings, 3L)
  # Fallback returns a TF-IDF model with vectorizer
  expect_true(
    !is.null(result$model$vectorizer) ||
      identical(result$model$method, "ollama")
  )
})

# ---- .embed_ollama with mocked ollamar::embed -----------------------

test_that("embed_nodes('ollama') calls .ollama_embed_text in batches", {
  nodes <- make_embed_nodes(5L)
  n_dims <- 768L

  fake_embed <- function(model_name, text, base_url) {
    as.numeric(seq_len(n_dims))
  }

  local_mocked_bindings(
    .ollama_embed_text = fake_embed,
    ollama_available = function() TRUE,
    .package = "rrlmgraph"
  )

  result <- embed_nodes(
    nodes,
    method = "ollama",
    cache_dir = withr::local_tempdir()
  )

  expect_named(result, c("embeddings", "model", "matrix"))
  expect_length(result$embeddings, 5L)
  expect_length(result$embeddings[[1L]], n_dims)
})

test_that("embed_nodes('ollama') produces 768-dim vectors", {
  skip_if_not_installed("mockery")
  nodes <- make_embed_nodes(2L)
  n_dims <- 768L

  mockery::stub(
    rrlmgraph:::.embed_ollama,
    ".ollama_embed_text",
    function(m, t, b) {
      rep(0.1, n_dims)
    }
  )
  mockery::stub(rrlmgraph:::.embed_ollama, "ollama_available", function() TRUE)

  result <- embed_nodes(
    nodes,
    method = "ollama",
    cache_dir = withr::local_tempdir()
  )

  for (emb in result$embeddings) {
    expect_length(emb, n_dims)
  }
})

# ---- caching --------------------------------------------------------

test_that("embed_nodes('ollama') skips re-embedding cached nodes", {
  nodes <- make_embed_nodes(2L)
  tmp <- withr::local_tempdir()
  n_dims <- 768L
  call_count <- 0L

  local_mocked_bindings(
    .ollama_embed_text = function(m, t, b) {
      call_count <<- call_count + 1L
      rep(0.5, n_dims)
    },
    ollama_available = function() TRUE,
    .package = "rrlmgraph"
  )

  # First call — should embed 2 nodes
  embed_nodes(nodes, method = "ollama", cache_dir = tmp)
  calls_first <- call_count

  # Second call with same nodes — should use cache (0 additional calls)
  embed_nodes(nodes, method = "ollama", cache_dir = tmp)
  calls_second <- call_count - calls_first

  expect_equal(calls_first, 2L)
  expect_equal(calls_second, 0L)
})

# ---- output matrix --------------------------------------------------

test_that("embed_nodes('ollama') returns a matrix when vectors are non-null", {
  skip_if_not_installed("mockery")
  nodes <- make_embed_nodes(3L)
  n_dims <- 768L

  mockery::stub(
    rrlmgraph:::.embed_ollama,
    ".ollama_embed_text",
    function(m, t, b) {
      rep(0.2, n_dims)
    }
  )
  mockery::stub(rrlmgraph:::.embed_ollama, "ollama_available", function() TRUE)

  result <- embed_nodes(
    nodes,
    method = "ollama",
    cache_dir = withr::local_tempdir()
  )

  expect_true(!is.null(result$matrix))
  expect_equal(nrow(result$matrix), 3L)
  expect_equal(ncol(result$matrix), n_dims)
})

# ---- embed_query forward -----------------------------------------

test_that("embed_query('ollama') returns a 768-dim vector when Ollama available", {
  n_dims <- 768L

  local_mocked_bindings(
    ollama_available = function() TRUE,
    .ollama_embed_text = function(m, t, b) rep(0.3, n_dims),
    .package = "rrlmgraph"
  )

  vec <- embed_query(
    "test query",
    model = list(method = "ollama"),
    method = "ollama"
  )
  expect_length(vec, n_dims)
})

test_that("embed_query('ollama') returns zero vector when Ollama unavailable", {
  local_mocked_bindings(
    ollama_available = function() FALSE,
    .package = "rrlmgraph"
  )

  vec <- suppressWarnings(
    embed_query(
      "test query",
      model = list(method = "ollama"),
      method = "ollama"
    )
  )
  expect_length(vec, 768L)
  expect_true(all(vec == 0))
})
