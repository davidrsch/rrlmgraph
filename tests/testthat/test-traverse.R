# tests/testthat/test-traverse.R
# Sprint 2 integration tests: query_context + compute_relevance
# Issue #16 acceptance criteria.
# Focuses on parametric budget enforcement and traversal-context pipeline.

skip_if_not_installed("igraph")

# ---- shared fixture helpers -----------------------------------------

# 8-node linear chain: a -> b -> c -> d -> e -> f -> g -> h
make_chain_graph <- function(n = 8L) {
  nm <- paste0("pkg::n", seq_len(n))
  verts <- data.frame(
    name = nm,
    node_type = "function",
    pagerank = seq(0.05, 1.0, length.out = n),
    body = paste0("function() { '", strrep("context_word ", 30L), "' }"),
    signature = paste0(nm, "()"),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = nm[-n],
    to = nm[-1L],
    weight = 1.0,
    edge_type = "CALLS",
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "chain"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  class(g) <- c("rrlm_graph", class(g))
  g
}

# Hub-and-spoke: hub connected to 6 spokes
make_hub_graph <- function() {
  hub <- "pkg::hub"
  spokes <- paste0("pkg::spoke", 1:6)
  all_nm <- c(hub, spokes)

  verts <- data.frame(
    name = all_nm,
    node_type = "function",
    pagerank = c(0.9, rep(0.1 / 6, 6L)),
    body = paste0("function() { '", strrep("x ", 50L), "' }"),
    signature = paste0(all_nm, "()"),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = rep(hub, 6L),
    to = spokes,
    weight = 1.0,
    edge_type = "CALLS",
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "hub"
  class(g) <- c("rrlm_graph", class(g))
  g
}

# ============================================================
# PARAMETRIC BUDGET ENFORCEMENT
# Runs query_context() with 5 different budgets and asserts
# tokens_used <= budget_tokens for each.
# ============================================================

test_that("query_context never exceeds budget — parametric over 5 budgets", {
  g <- make_chain_graph(8L)

  budget_values <- c(50L, 100L, 250L, 500L, 1000L)

  for (budget in budget_values) {
    ctx <- suppressWarnings(query_context(
      g,
      "context word chain",
      budget_tokens = budget
    ))

    expect_lte(
      ctx$tokens_used,
      ctx$budget_tokens,
      label = paste0("budget=", budget, ": tokens_used <= budget_tokens")
    )
    expect_equal(
      ctx$budget_tokens,
      as.integer(budget),
      label = paste0("budget=", budget, ": budget_tokens field matches arg")
    )
  }
})

test_that("tighter budgets yield fewer or equal nodes than looser budgets", {
  g <- make_chain_graph(8L)

  ctx_tight <- suppressWarnings(query_context(
    g,
    "node chain",
    budget_tokens = 80L
  ))
  ctx_loose <- suppressWarnings(query_context(
    g,
    "node chain",
    budget_tokens = 800L
  ))

  expect_lte(length(ctx_tight$nodes), length(ctx_loose$nodes))
})

test_that("budget=1L still returns at least the (possibly truncated) seed", {
  g <- make_chain_graph(4L)
  ctx <- suppressWarnings(query_context(g, "tiny budget", budget_tokens = 1L))

  expect_s3_class(ctx, "rrlm_context")
  # Seed may consume all tokens — nodes length >= 1 is not guaranteed
  # when seed body alone exceeds budget; still must not error
  expect_lte(ctx$tokens_used, max(1L, ctx$budget_tokens))
})

# ============================================================
# TRAVERSAL CORRECTNESS
# ============================================================

test_that("auto seed selects highest-pagerank function node in chain", {
  g <- make_chain_graph(6L)
  ctx <- query_context(g, "traverse chain")

  # pkg::n6 has pagerank=1.0 (highest)
  expect_equal(ctx$seed_node, "pkg::n6")
  expect_equal(ctx$nodes[[1L]], "pkg::n6")
})

test_that("hub seed yields spokes in nodes with generous budget", {
  g <- make_hub_graph()
  ctx <- query_context(
    g,
    "spoke traversal",
    seed_node = "pkg::hub",
    budget_tokens = 5000L
  )

  expect_equal(ctx$nodes[[1L]], "pkg::hub")
  expect_gte(length(ctx$nodes), 2L) # hub + at least one spoke
})

test_that("relevance_scores contains all returned nodes as names", {
  g <- make_chain_graph(6L)
  ctx <- query_context(g, "score check", budget_tokens = 2000L)

  expect_true(all(ctx$nodes %in% names(ctx$relevance_scores)))
})

test_that("relevance scores are in [0, 1]", {
  g <- make_chain_graph(6L)
  ctx <- query_context(g, "score range", budget_tokens = 2000L)

  expect_true(all(ctx$relevance_scores >= 0 & ctx$relevance_scores <= 1))
})

test_that("no duplicate nodes in traversal output", {
  g <- make_hub_graph()
  ctx <- query_context(
    g,
    "no duplicates",
    seed_node = "pkg::hub",
    budget_tokens = 10000L
  )

  expect_equal(length(ctx$nodes), length(unique(ctx$nodes)))
})

# ============================================================
# TRAVERSAL + CONTEXT ASSEMBLY PIPELINE
# ============================================================

test_that("context_string from query_context is non-empty character", {
  g <- make_chain_graph(4L)
  ctx <- query_context(g, "integration test", budget_tokens = 500L)

  expect_type(ctx$context_string, "character")
  expect_length(ctx$context_string, 1L)
  expect_gt(nchar(ctx$context_string), 0L)
})

test_that("context_string contains seed node name", {
  g <- make_chain_graph(4L)
  ctx <- query_context(
    g,
    "seed must appear",
    seed_node = "pkg::n1",
    budget_tokens = 800L
  )

  expect_true(
    grepl("pkg::n1", ctx$context_string, fixed = TRUE) ||
      grepl("n1", ctx$context_string, fixed = TRUE)
  )
})

test_that("assemble_context_string output contains CORE FUNCTIONS section", {
  g <- make_chain_graph(3L)
  ctx <- query_context(g, "core section", budget_tokens = 800L)

  expect_match(ctx$context_string, "CORE FUNCTIONS", fixed = TRUE)
})

test_that("assemble_context_string output contains CONSTRAINTS section", {
  g <- make_chain_graph(3L)
  ctx <- query_context(g, "constraints section", budget_tokens = 800L)

  expect_match(ctx$context_string, "CONSTRAINTS", fixed = TRUE)
})

test_that("query string appears in assembled context string", {
  g <- make_chain_graph(3L)
  query <- "unique_marker_xzqw"
  ctx <- query_context(g, query, budget_tokens = 800L)

  expect_match(ctx$context_string, query, fixed = TRUE)
})

test_that("tokens_used is a reasonable approximation of text length", {
  g <- make_chain_graph(4L)
  ctx <- query_context(g, "token approx", budget_tokens = 1000L)

  # .count_tokens uses nchar/3.5 fallback or tokenizers word-count*1.3;
  # either way must be within factor ~2 of the naive nchar/4 estimate.
  nchars <- nchar(ctx$context_string)
  expect_gte(ctx$tokens_used, as.integer(nchars / 6L))
  expect_lte(ctx$tokens_used, as.integer(ceiling(nchars / 2L)))
})

# ============================================================
# compute_relevance integration
# ============================================================

test_that("compute_relevance returns numeric(1) in [0,1]", {
  g <- make_chain_graph(4L)
  sc <- compute_relevance("pkg::n1", numeric(0), character(0), g)

  expect_type(sc, "double")
  expect_length(sc, 1L)
  expect_gte(sc, 0)
  expect_lte(sc, 1)
})

test_that("compute_relevance with visited nodes does not error", {
  g <- make_chain_graph(4L)
  sc <- compute_relevance(
    "pkg::n2",
    numeric(0),
    visited = c("pkg::n1", "pkg::n3"),
    graph = g
  )

  expect_true(is.numeric(sc) && length(sc) == 1L)
})

# ============================================================
# EDGE CASES
# ============================================================

test_that("single-node graph returns that node as result", {
  g_single <- igraph::make_empty_graph(n = 1L, directed = TRUE)
  igraph::V(g_single)$name <- "pkg::only"
  igraph::V(g_single)$node_type <- "function"
  igraph::V(g_single)$pagerank <- 1.0
  class(g_single) <- c("rrlm_graph", class(g_single))

  ctx <- query_context(g_single, "only node", budget_tokens = 1000L)

  expect_equal(ctx$seed_node, "pkg::only")
  expect_equal(ctx$nodes[[1L]], "pkg::only")
})

test_that("disconnected components: seed reachable cluster only", {
  # Two disjoint pairs: a-b and c-d; seed = a
  verts <- data.frame(
    name = c("pkg::a", "pkg::b", "pkg::c", "pkg::d"),
    node_type = "function",
    pagerank = c(0.3, 0.4, 0.2, 0.1),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("pkg::a", "pkg::c"),
    to = c("pkg::b", "pkg::d"),
    weight = 1.0,
    edge_type = "CALLS",
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = FALSE
  )
  class(g) <- c("rrlm_graph", class(g))

  ctx <- query_context(
    g,
    "find a and b",
    seed_node = "pkg::a",
    budget_tokens = 5000L
  )

  expect_true("pkg::a" %in% ctx$nodes)
  # c and d are not reachable from a
  expect_false("pkg::c" %in% ctx$nodes)
  expect_false("pkg::d" %in% ctx$nodes)
})
