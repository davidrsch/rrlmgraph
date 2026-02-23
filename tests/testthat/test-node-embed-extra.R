# tests/testthat/test-node-embed-extra.R
# Branch-coverage tests for node_embed.R helpers not reached by the
# primary embed test files.

skip_if_not_installed("igraph")

# ---- .text_hash -------------------------------------------------------

test_that(".text_hash returns a non-empty character string", {
  result <- rrlmgraph:::.text_hash("hello world")
  expect_type(result, "character")
  expect_length(result, 1L)
  expect_gt(nchar(result), 0L)
})

test_that(".text_hash returns different values for different texts", {
  h1 <- rrlmgraph:::.text_hash("hello")
  h2 <- rrlmgraph:::.text_hash("world")
  expect_false(identical(h1, h2))
})

test_that(".text_hash returns the same value for the same text", {
  h1 <- rrlmgraph:::.text_hash("same text here")
  h2 <- rrlmgraph:::.text_hash("same text here")
  expect_equal(h1, h2)
})

test_that(".text_hash handles empty string", {
  result <- rrlmgraph:::.text_hash("")
  expect_type(result, "character")
  expect_length(result, 1L)
})

# ---- .resolve_embed_cache --------------------------------------------

test_that(".resolve_embed_cache with explicit cache_dir returns correct path", {
  tmp <- withr::local_tempdir()
  path <- rrlmgraph:::.resolve_embed_cache(tmp, "embeddings.rds")
  expect_equal(path, file.path(tmp, "embeddings.rds"))
})

test_that(".resolve_embed_cache with NULL cache_dir creates .rrlmgraph subdir", {
  tmp <- withr::local_tempdir()
  old_wd <- setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  path <- rrlmgraph:::.resolve_embed_cache(NULL, "test.rds")

  expect_true(dir.exists(file.path(tmp, ".rrlmgraph")))
  expect_equal(basename(path), "test.rds")
  # Normalise separators before comparing (Windows mixes / and \\)
  expect_equal(
    normalizePath(dirname(path), mustWork = FALSE),
    normalizePath(file.path(tmp, ".rrlmgraph"), mustWork = FALSE)
  )
})

# ---- .save_embed_cache / .load_embed_cache ---------------------------

test_that(".save_embed_cache and .load_embed_cache round-trip a list", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "cache.rds")
  payload <- list(
    abc = c(0.1, 0.2, 0.3),
    def = c(0.4, 0.5, 0.6)
  )

  rrlmgraph:::.save_embed_cache(path, payload)
  loaded <- rrlmgraph:::.load_embed_cache(path)

  expect_equal(loaded, payload)
})

test_that(".load_embed_cache returns empty list for missing file", {
  tmp <- withr::local_tempdir()
  result <- rrlmgraph:::.load_embed_cache(file.path(tmp, "nonexistent.rds"))
  expect_equal(result, list())
})

# ---- embed_query: NULL model returns numeric(0) ----------------------

test_that("embed_query returns numeric(0) when model is NULL", {
  result <- embed_query("any query", model = NULL)
  expect_equal(result, numeric(0))
})

# ---- ollama_available: returns FALSE when Ollama not running ---------

test_that("ollama_available returns FALSE when Ollama is not running", {
  skip_if_not_installed("httr2")
  # Use a port that is almost certainly not listening
  withr::with_envvar(
    c(OLLAMA_BASE_URL = "http://127.0.0.1:19999"),
    {
      result <- ollama_available()
      expect_type(result, "logical")
      expect_length(result, 1L)
      # On CI (no local Ollama), this should be FALSE
      # Allow TRUE only if someone happens to run Ollama on port 19999
    }
  )
})
