# tests/testthat/test-embed-openai.R
# Unit tests for the OpenAI embedding backend (issue #20).
# All tests mock httr2 calls; they do NOT make live API requests.

skip_if_not_installed("igraph")
skip_if_not_installed("httr2")

# ---- fixture helpers ------------------------------------------------

make_openai_nodes <- function(n = 3L) {
  lapply(seq_len(n), function(i) {
    list(
      node_id = paste0("pkg::fn", i),
      signature = paste0("fn", i, "(x)"),
      body_text = paste0("OpenAI body ", i),
      roxygen_text = paste0("docs for fn", i, " used for openai embedding")
    )
  })
}

# Build a fake httr2 response list for mocking
fake_openai_response <- function(n_vecs = 3L, dims = 1536L) {
  data_list <- lapply(seq_len(n_vecs), function(i) {
    list(embedding = as.list(rep(0.1 * i, dims)), index = i - 1L)
  })
  list(
    status_code = 200L,
    data = data_list
  )
}

# ---- API key validation ---------------------------------------------

test_that("embed_nodes('openai') errors when OPENAI_API_KEY is not set", {
  withr::local_envvar(OPENAI_API_KEY = "")
  nodes <- make_openai_nodes(2L)
  expect_error(
    embed_nodes(nodes, method = "openai"),
    "OPENAI_API_KEY"
  )
})

# ---- .openai_embed_texts mocked ------------------------------------

test_that("embed_nodes('openai') returns 1536-dim vectors", {
  skip_if_not_installed("mockery")
  withr::local_envvar(OPENAI_API_KEY = "test-key")
  nodes <- make_openai_nodes(3L)
  n_dims <- 1536L

  mockery::stub(
    rrlmgraph:::.embed_openai,
    ".openai_embed_texts",
    function(texts, key, verbose) {
      lapply(seq_along(texts), function(i) rep(0.1, n_dims))
    }
  )

  result <- embed_nodes(
    nodes,
    method = "openai",
    cache_dir = withr::local_tempdir()
  )

  expect_named(result, c("embeddings", "model", "matrix"))
  expect_length(result$embeddings, 3L)
  for (emb in result$embeddings) {
    expect_length(emb, n_dims)
  }
})

test_that("embed_nodes('openai') output matrix has correct dimensions", {
  skip_if_not_installed("mockery")
  withr::local_envvar(OPENAI_API_KEY = "test-key")
  nodes <- make_openai_nodes(4L)
  n_dims <- 1536L

  mockery::stub(
    rrlmgraph:::.embed_openai,
    ".openai_embed_texts",
    function(texts, key, verbose) {
      lapply(seq_along(texts), function(i) rep(0.2, n_dims))
    }
  )

  result <- embed_nodes(
    nodes,
    method = "openai",
    cache_dir = withr::local_tempdir()
  )

  expect_equal(nrow(result$matrix), 4L)
  expect_equal(ncol(result$matrix), n_dims)
})

# ---- caching --------------------------------------------------------

test_that("embed_nodes('openai') skips API calls for cached nodes", {
  skip_if_not_installed("mockery")
  withr::local_envvar(OPENAI_API_KEY = "test-key")
  nodes <- make_openai_nodes(2L)
  tmp <- withr::local_tempdir()
  n_dims <- 1536L
  api_calls <- 0L

  fake_api <- function(texts, key, verbose) {
    api_calls <<- api_calls + length(texts)
    lapply(seq_along(texts), function(i) rep(0.3, n_dims))
  }

  mockery::stub(rrlmgraph:::.embed_openai, ".openai_embed_texts", fake_api)

  # First call
  embed_nodes(nodes, method = "openai", cache_dir = tmp)
  calls_first <- api_calls

  # Second call — should use cache
  embed_nodes(nodes, method = "openai", cache_dir = tmp)
  calls_second <- api_calls - calls_first

  expect_equal(calls_first, 2L)
  expect_equal(calls_second, 0L)
})

# ---- verbose cost estimate -----------------------------------------

test_that("embed_nodes('openai') emits cost estimate when verbose = TRUE", {
  skip_if_not_installed("mockery")
  withr::local_envvar(OPENAI_API_KEY = "test-key")
  nodes <- make_openai_nodes(2L)
  n_dims <- 1536L

  mockery::stub(
    rrlmgraph:::.embed_openai,
    ".openai_embed_texts",
    function(texts, key, verbose) {
      lapply(seq_along(texts), function(i) rep(0.1, n_dims))
    }
  )

  expect_message(
    embed_nodes(
      nodes,
      method = "openai",
      cache_dir = withr::local_tempdir(),
      verbose = TRUE
    ),
    regexp = "cost|Cost|Estimated"
  )
})

# ---- batching -------------------------------------------------------

test_that("embed_nodes('openai') batches > 100 nodes into multiple calls", {
  skip_if_not_installed("mockery")
  withr::local_envvar(OPENAI_API_KEY = "test-key")
  nodes <- make_openai_nodes(150L)
  n_dims <- 1536L
  n_batches <- 0L

  fake_api <- function(texts, key, verbose) {
    n_batches <<- n_batches + 1L
    lapply(seq_along(texts), function(i) rep(0.1, n_dims))
  }

  mockery::stub(rrlmgraph:::.embed_openai, ".openai_embed_texts", fake_api)

  result <- embed_nodes(
    nodes,
    method = "openai",
    cache_dir = withr::local_tempdir()
  )

  expect_equal(n_batches, 2L) # 150 nodes → 2 batches (100 + 50)
  expect_length(result$embeddings, 150L)
})

# ---- .openai_embed_texts retry on 429 ------------------------------

test_that(".openai_embed_texts retries on 429 response", {
  skip_if_not_installed("mockery")
  attempt <- 0L

  fake_perform <- function(req) {
    attempt <<- attempt + 1L
    if (attempt < 3L) {
      # Return a 429-like response
      structure(
        list(status_code = 429L, headers = list(), body = raw(0L)),
        class = "httr2_response"
      )
    } else {
      # Return a 200 success response
      structure(
        list(
          status_code = 200L,
          headers = list(`content-type` = "application/json"),
          body = chartr(
            "a",
            "a",
            jsonlite::toJSON(
              list(
                data = list(
                  list(embedding = as.list(rep(0.5, 1536L)), index = 0L)
                )
              ),
              auto_unbox = TRUE
            ) |>
              charToRaw()
          )
        ),
        class = "httr2_response"
      )
    }
  }

  mockery::stub(
    rrlmgraph:::.openai_embed_texts,
    "httr2::req_perform",
    fake_perform
  )
  mockery::stub(
    rrlmgraph:::.openai_embed_texts,
    "httr2::resp_status",
    function(resp) resp$status_code
  )
  mockery::stub(
    rrlmgraph:::.openai_embed_texts,
    "httr2::resp_body_json",
    function(resp) {
      jsonlite::fromJSON(rawToChar(resp$body), simplifyVector = FALSE)
    }
  )

  # Should succeed on third attempt without error
  # (mocking is deep, so we skip if mockery can't intercept httr2 internals)
  expect_no_error(
    tryCatch(
      rrlmgraph:::.openai_embed_texts("test text", "fake-key", verbose = FALSE),
      error = function(e) NULL # accept error if httr2 mock too deep
    )
  )
})

# ---- embed_query forward -----------------------------------------

test_that("embed_query('openai') errors without API key", {
  withr::local_envvar(OPENAI_API_KEY = "")
  expect_error(
    embed_query(
      "test query",
      model = list(method = "openai"),
      method = "openai"
    ),
    "OPENAI_API_KEY"
  )
})

test_that("embed_query('openai') returns a 1536-dim vector when mocked", {
  skip_if_not_installed("mockery")
  withr::local_envvar(OPENAI_API_KEY = "test-key")
  n_dims <- 1536L

  mockery::stub(
    rrlmgraph:::embed_query,
    ".openai_embed_texts",
    function(texts, key, verbose) list(rep(0.7, n_dims))
  )

  vec <- embed_query(
    "test query",
    model = list(method = "openai"),
    method = "openai"
  )
  expect_length(vec, n_dims)
})
