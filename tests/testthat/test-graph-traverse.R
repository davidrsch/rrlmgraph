# tests/testthat/test-graph-traverse.R
# Unit tests for compute_relevance() and update_task_weights()
# Issue #10 acceptance criteria.

skip_if_not_installed("igraph")

# ---- fixture helpers ------------------------------------------------

make_traverse_graph <- function() {
  # 4 user-function nodes, 1 package node; a few CALLS edges
  verts <- data.frame(
    name = c("pkg::a", "pkg::b", "pkg::c", "pkg::d", "dplyr"),
    node_type = c("function", "function", "function", "function", "package"),
    pagerank = c(0.10, 0.40, 0.30, 0.20, 0.05),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("pkg::a", "pkg::b", "pkg::c"),
    to = c("pkg::b", "pkg::c", "pkg::d"),
    weight = c(1.0, 1.0, 1.0),
    edge_type = c("CALLS", "CALLS", "CALLS"),
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

make_graph_with_cochange <- function() {
  verts <- data.frame(
    name = c("pkg::x", "pkg::y", "pkg::z"),
    node_type = "function",
    pagerank = c(0.33, 0.33, 0.34),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("pkg::x", "pkg::y"),
    to = c("pkg::y", "pkg::z"),
    weight = c(0.9, 0.8),
    edge_type = c("CO_CHANGES", "CO_CHANGES"),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = FALSE
  )
  igraph::graph_attr(g, "project_name") <- "copkg"
  class(g) <- c("rrlm_graph", class(g))
  g
}

# ---- compute_relevance: return type / range -------------------------

test_that("compute_relevance returns a single numeric in [0, 1]", {
  g <- make_traverse_graph()
  s <- compute_relevance("pkg::a", query_vec = numeric(0), graph = g)

  expect_type(s, "double")
  expect_length(s, 1L)
  expect_gte(s, 0)
  expect_lte(s, 1)
})

test_that("compute_relevance returns 0 for unknown node with a warning", {
  g <- make_traverse_graph()
  expect_warning(
    s <- compute_relevance("NOTHERE", query_vec = numeric(0), graph = g),
    regexp = NULL
  )
  expect_equal(s, 0)
})

# ---- cold start: no task history, no visited ------------------------

test_that("cold start still returns value in [0, 1]", {
  g <- make_traverse_graph()
  q <- numeric(0)
  scores <- vapply(
    c("pkg::a", "pkg::b", "pkg::c", "pkg::d"),
    compute_relevance,
    numeric(1),
    query_vec = q,
    visited = character(0),
    graph = g
  )
  expect_true(all(scores >= 0 & scores <= 1))
})

test_that("cold start: node with highest pagerank scores higher (no sem)", {
  g <- make_traverse_graph()
  # pkg::b has highest pagerank (0.40); no semantic signal
  s_b <- compute_relevance("pkg::b", numeric(0), graph = g)
  s_d <- compute_relevance("pkg::d", numeric(0), graph = g)
  expect_gte(s_b, s_d)
})

# ---- semantic signal ------------------------------------------------

test_that("non-zero query_vec changes score vs numeric(0)", {
  skip_if_not_installed("text2vec")
  g <- make_traverse_graph()
  # Use a multi-node corpus so IDF weights are non-trivial and cosine
  # similarity is meaningful (single-doc TF-IDF collapses to all-equal IDF)
  nodes <- list(
    list(
      node_id = "pkg::b",
      name = "b",
      signature = "increment_value(x)",
      body_text = "add one to numeric value x returning incremented result",
      roxygen_text = "increment a number by one"
    ),
    list(
      node_id = "pkg::other",
      name = "other",
      signature = "filter_dataframe(df)",
      body_text = "remove rows where score column is missing using filter",
      roxygen_text = "filter rows from a dataframe"
    ),
    list(
      node_id = "pkg::another",
      name = "another",
      signature = "fit_model(train)",
      body_text = "fit linear regression model to training data",
      roxygen_text = "build a regression model"
    )
  )
  emb <- embed_nodes(nodes)
  igraph::V(g)$embedding <- vector("list", igraph::vcount(g))
  igraph::V(g)$embedding[[which(igraph::V(g)$name == "pkg::b")]] <-
    emb$embeddings[["pkg::b"]]
  q <- embed_query("increment a number", emb$model)

  s_with_q <- compute_relevance("pkg::b", q, graph = g)
  s_no_q <- compute_relevance("pkg::b", numeric(0), graph = g)
  # Scores must be in [0,1]
  expect_gte(s_with_q, 0)
  expect_lte(s_with_q, 1)
  # With a semantically matching query the score must exceed the no-query score
  expect_gt(s_with_q, s_no_q)
})

# ---- co-change signal -----------------------------------------------

test_that("cochange_score is 0 when visited is empty", {
  g <- make_graph_with_cochange()
  # Force no semantic, uniform pagerank → score driven by default weights
  s_empty <- compute_relevance(
    "pkg::x",
    numeric(0),
    visited = character(0),
    graph = g
  )
  s_visited <- compute_relevance(
    "pkg::x",
    numeric(0),
    visited = "pkg::y",
    graph = g
  )
  # With pkg::y visited there is a CO_CHANGES edge (weight 0.9), so score goes up
  expect_gte(s_visited, s_empty)
})

test_that("cochange_score improves score when neighbor was visited", {
  g <- make_graph_with_cochange()
  s_before <- compute_relevance(
    "pkg::x",
    numeric(0),
    visited = character(0),
    graph = g
  )
  s_after <- compute_relevance(
    "pkg::x",
    numeric(0),
    visited = "pkg::y",
    graph = g
  )
  expect_gt(s_after, s_before)
})

# ---- weight configuration -------------------------------------------

test_that("score changes when weights are overridden at call level", {
  g <- make_traverse_graph()
  s_default <- compute_relevance("pkg::b", numeric(0), graph = g)
  # Force all weight onto pagerank (0.25 → 1.0)
  s_pagerank <- compute_relevance(
    "pkg::b",
    numeric(0),
    graph = g,
    weights = list(semantic = 0, pagerank = 1, task_trace = 0, cochange = 0)
  )
  expect_gte(s_pagerank, 0)
  expect_lte(s_pagerank, 1)
  expect_false(isTRUE(all.equal(s_default, s_pagerank)))
})

test_that("score changes when weights set via option", {
  g <- make_traverse_graph()
  old_opt <- getOption("rrlmgraph.weights")
  on.exit(options(rrlmgraph.weights = old_opt), add = TRUE)

  options(
    rrlmgraph.weights = list(
      semantic = 0,
      pagerank = 1,
      task_trace = 0,
      cochange = 0
    )
  )
  s_opt <- compute_relevance("pkg::b", numeric(0), graph = g)
  options(rrlmgraph.weights = NULL)
  s_default <- compute_relevance("pkg::b", numeric(0), graph = g)

  expect_false(isTRUE(all.equal(s_opt, s_default)))
})

# ---- task_trace_weight attribute ------------------------------------

test_that("task_trace_weight attribute read correctly (non-default)", {
  g <- make_traverse_graph()
  # Manually set task_trace_weight on pkg::b to 1.0 (maximum)
  igraph::V(g)$task_trace_weight <- rep(0.5, igraph::vcount(g))
  idx <- which(igraph::V(g)$name == "pkg::b")
  igraph::V(g)$task_trace_weight[idx] <- 1.0

  s_boosted <- compute_relevance(
    "pkg::b",
    numeric(0),
    weights = list(semantic = 0, pagerank = 0, task_trace = 1, cochange = 0),
    graph = g
  )
  expect_equal(s_boosted, 1.0)
})

# ---- update_task_weights --------------------------------------------

test_that("update_task_weights returns graph with same class", {
  g <- make_traverse_graph()
  g2 <- update_task_weights(g, useful_nodes = "pkg::a")
  expect_s3_class(g2, "rrlm_graph")
  expect_s3_class(g2, "igraph")
})

test_that("update_task_weights creates task_trace_weight attribute", {
  g <- make_traverse_graph()
  expect_null(igraph::V(g)$task_trace_weight)
  g2 <- update_task_weights(g, useful_nodes = character(0))
  expect_false(is.null(igraph::V(g2)$task_trace_weight))
})

test_that("update_task_weights increases weight for useful node", {
  g <- make_traverse_graph()
  g <- update_task_weights(g, useful_nodes = character(0)) # initialise
  before <- igraph::V(g)$task_trace_weight[igraph::V(g)$name == "pkg::a"]
  g2 <- update_task_weights(g, useful_nodes = "pkg::a")
  after <- igraph::V(g2)$task_trace_weight[igraph::V(g2)$name == "pkg::a"]
  expect_gt(after, before)
})

test_that("update_task_weights all weights remain in [0, 1]", {
  g <- make_traverse_graph()
  g2 <- update_task_weights(g, useful_nodes = c("pkg::a", "pkg::b"))
  tw <- igraph::V(g2)$task_trace_weight
  expect_true(all(tw >= 0 & tw <= 1))
})

test_that("update_task_weights does not error for empty useful_nodes", {
  g <- make_traverse_graph()
  expect_no_error(update_task_weights(g, useful_nodes = character(0)))
})

test_that("update_task_weights ignores unknown node names gracefully", {
  g <- make_traverse_graph()
  expect_no_error(update_task_weights(g, useful_nodes = c("NOTEXIST")))
})
