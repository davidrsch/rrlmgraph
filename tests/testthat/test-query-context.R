# tests/testthat/test-query-context.R
# Unit tests for query_context(), print.rrlm_context(), summary.rrlm_context()
# Issue #9 acceptance criteria.

skip_if_not_installed("igraph")

# ---- fixture helpers ------------------------------------------------

make_qc_graph <- function() {
  # 5 function nodes chained a -> b -> c -> d, plus isolated node "e"
  verts <- data.frame(
    name = c("pkg::a", "pkg::b", "pkg::c", "pkg::d", "pkg::e"),
    node_type = c("function", "function", "function", "function", "function"),
    pagerank = c(0.10, 0.40, 0.25, 0.20, 0.05),
    body = c(
      "function() { b() }",
      "function() { c() }",
      "function() { d() }",
      "function() { invisible(NULL) }",
      "function() { 42L }"
    ),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("pkg::a", "pkg::b", "pkg::c"),
    to = c("pkg::b", "pkg::c", "pkg::d"),
    weight = c(1.0, 1.0, 1.0),
    edge_type = rep("CALLS", 3L),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "testpkg"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0.1
  class(g) <- c("rrlm_graph", class(g))
  g
}

make_dense_graph <- function(n = 10L) {
  # n function nodes, fully connected; used for budget / max_nodes tests
  nm <- paste0("pkg::f", seq_len(n))
  verts <- data.frame(
    name = nm,
    node_type = "function",
    pagerank = seq(0.1, 1.0, length.out = n),
    body = paste0("function() { '", strrep("x", 200L), "' }"),
    stringsAsFactors = FALSE
  )
  edges_from <- nm[rep(seq_len(n - 1L), each = 1L)]
  edges_to <- nm[seq(2L, n)]
  edges <- data.frame(
    from = edges_from,
    to = edges_to,
    weight = 1.0,
    edge_type = "CALLS",
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "densepkg"
  class(g) <- c("rrlm_graph", class(g))
  g
}

# ---- return structure -----------------------------------------------

test_that("query_context returns rrlm_context with correct fields", {
  g <- make_qc_graph()
  ctx <- query_context(g, "load data")

  expect_s3_class(ctx, "rrlm_context")
  expect_true(is.list(ctx))
  expect_named(
    ctx,
    c(
      "nodes",
      "context_string",
      "tokens_used",
      "budget_tokens",
      "seed_node",
      "relevance_scores"
    ),
    ignore.order = TRUE
  )
})

test_that("$nodes is a character vector with at least 1 element", {
  g <- make_qc_graph()
  ctx <- query_context(g, "run model training")

  expect_type(ctx$nodes, "character")
  expect_gte(length(ctx$nodes), 1L)
})

test_that("$context_string is a single non-NA character", {
  g <- make_qc_graph()
  ctx <- query_context(g, "fetch results")

  expect_type(ctx$context_string, "character")
  expect_length(ctx$context_string, 1L)
  expect_false(is.na(ctx$context_string))
})

test_that("$tokens_used and $budget_tokens are integers", {
  g <- make_qc_graph()
  ctx <- query_context(g, "parse file")

  expect_type(ctx$tokens_used, "integer")
  expect_type(ctx$budget_tokens, "integer")
})

test_that("$relevance_scores is a named numeric vector", {
  g <- make_qc_graph()
  ctx <- query_context(g, "compute residuals")

  expect_type(ctx$relevance_scores, "double")
  expect_false(is.null(names(ctx$relevance_scores)))
  expect_true(all(ctx$relevance_scores >= 0 & ctx$relevance_scores <= 1))
})

# ---- hard budget constraint -----------------------------------------

test_that("tokens_used never exceeds budget_tokens (hard constraint)", {
  g <- make_dense_graph(10L)
  ctx <- query_context(g, "evaluate predictions", budget_tokens = 100L)

  expect_lte(ctx$tokens_used, ctx$budget_tokens)
})

test_that("tokens_used never exceeds budget with very tight budget", {
  g <- make_qc_graph()
  ctx <- query_context(g, "train model", budget_tokens = 20L)

  expect_lte(ctx$tokens_used, 20L)
})

test_that("budget_tokens in return matches the supplied argument", {
  g <- make_qc_graph()
  ctx <- query_context(g, "summarise results", budget_tokens = 500L)

  expect_equal(ctx$budget_tokens, 500L)
})

# ---- seed node handling ---------------------------------------------

test_that("auto seed selects the function node with highest pagerank", {
  g <- make_qc_graph()
  ctx <- query_context(g, "invoke b")

  # pkg::b has highest pagerank (0.40) among function nodes
  expect_equal(ctx$seed_node, "pkg::b")
  expect_equal(ctx$nodes[[1L]], "pkg::b")
})

test_that("explicit seed_node overrides auto selection", {
  g <- make_qc_graph()
  ctx <- query_context(g, "use a", seed_node = "pkg::a")

  expect_equal(ctx$seed_node, "pkg::a")
  expect_equal(ctx$nodes[[1L]], "pkg::a")
})

test_that("explicit seed_node always appears first in $nodes", {
  g <- make_qc_graph()
  ctx <- query_context(g, "call c via b", seed_node = "pkg::c")

  expect_equal(ctx$nodes[[1L]], "pkg::c")
})

test_that("invalid seed_node raises an error", {
  g <- make_qc_graph()

  expect_error(
    query_context(g, "x", seed_node = "pkg::nonexistent"),
    regexp = "not a vertex"
  )
})

# ---- max_nodes cap --------------------------------------------------

test_that("number of non-seed nodes never exceeds max_nodes", {
  g <- make_dense_graph(10L)
  ctx <- query_context(g, "iterate", budget_tokens = 50000L, max_nodes = 3L)

  # total nodes (including seed) <= max_nodes + 1
  expect_lte(length(ctx$nodes), 3L + 1L)
})

# ---- empty / trivial query ------------------------------------------

test_that("empty query string returns at least the seed node", {
  g <- make_qc_graph()
  ctx <- query_context(g, "")

  expect_gte(length(ctx$nodes), 1L)
})

test_that("single-character query returns valid rrlm_context", {
  g <- make_qc_graph()
  ctx <- query_context(g, "a")

  expect_s3_class(ctx, "rrlm_context")
  expect_gte(length(ctx$nodes), 1L)
})

# ---- isolated node --------------------------------------------------

test_that("isolated seed node (no edges) returns exactly that seed", {
  g <- make_qc_graph()
  ctx <- query_context(g, "evaluate e", seed_node = "pkg::e")

  expect_equal(ctx$seed_node, "pkg::e")
  expect_equal(ctx$nodes[[1L]], "pkg::e")
  expect_lte(ctx$tokens_used, ctx$budget_tokens)
})

# ---- empty graph ----------------------------------------------------

test_that("empty graph returns an rrlm_context with zero nodes", {
  g_empty <- igraph::make_empty_graph(n = 0L, directed = TRUE)
  class(g_empty) <- c("rrlm_graph", class(g_empty))

  ctx <- query_context(g_empty, "something")

  expect_s3_class(ctx, "rrlm_context")
  expect_length(ctx$nodes, 0L)
  expect_equal(ctx$tokens_used, 0L)
})

# ---- verbose flag does not change result ----------------------------

test_that("verbose=TRUE returns same structure as verbose=FALSE", {
  g <- make_qc_graph()
  ctx1 <- query_context(g, "train", verbose = FALSE)
  ctx2 <- suppressMessages(query_context(g, "train", verbose = TRUE))

  expect_equal(ctx1$nodes, ctx2$nodes)
  expect_equal(ctx1$seed_node, ctx2$seed_node)
  expect_equal(ctx1$budget_tokens, ctx2$budget_tokens)
})

# ---- S3 print / summary ---------------------------------------------

test_that("print.rrlm_context produces visible output", {
  g <- make_qc_graph()
  ctx <- query_context(g, "check output")

  out <- capture.output(print(ctx))
  expect_true(length(out) > 0L)
})

test_that("print.rrlm_context returns x invisibly", {
  g <- make_qc_graph()
  ctx <- query_context(g, "invisible return")

  ret <- withVisible(print(ctx))
  expect_false(ret$visible)
  expect_identical(ret$value, ctx)
})

test_that("summary.rrlm_context produces visible output", {
  g <- make_qc_graph()
  ctx <- query_context(g, "summarise")

  out <- capture.output(summary(ctx))
  expect_true(length(out) > 0L)
})

test_that("summary.rrlm_context returns object invisibly", {
  g <- make_qc_graph()
  ctx <- query_context(g, "invisible summary")

  ret <- withVisible(summary(ctx))
  expect_false(ret$visible)
  expect_identical(ret$value, ctx)
})
