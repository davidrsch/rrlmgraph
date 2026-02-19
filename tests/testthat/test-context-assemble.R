# tests/testthat/test-context-assemble.R
# Unit tests for build_node_context() and assemble_context_string()
# Issues #11 and #12 acceptance criteria.

skip_if_not_installed("igraph")

# ---- fixture helpers ------------------------------------------------

make_ctx_graph <- function() {
  verts <- data.frame(
    name = c("pkg::load_data", "pkg::clean_data", "pkg::fit_model", "dplyr"),
    node_type = c("function", "function", "function", "package"),
    pagerank = c(0.20, 0.35, 0.30, 0.15),
    signature = c(
      "load_data(path)",
      "clean_data(df, threshold = 0.05)",
      "fit_model(train, formula)",
      "dplyr"
    ),
    body_text = c(
      "if (!file.exists(path)) stop('not found')\nread.csv(path)",
      "df[!is.na(df$score), ]",
      "stats::lm(formula = formula, data = train)",
      ""
    ),
    roxygen_text = c(
      "#' Load a CSV file from disk\n#' @param path path to file",
      "#' Clean rows with missing scores",
      "#' Fit a linear model",
      ""
    ),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("pkg::clean_data", "pkg::fit_model"),
    to = c("pkg::load_data", "pkg::clean_data"),
    weight = c(1.0, 1.0),
    edge_type = c("CALLS", "CALLS"),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "mypkg"
  igraph::graph_attr(g, "r_version") <- "4.4.0"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0.1
  class(g) <- c("rrlm_graph", class(g))
  g
}

# ---- build_node_context: return type --------------------------------

test_that("build_node_context returns a single character", {
  g <- make_ctx_graph()
  out <- build_node_context("pkg::load_data", g)
  expect_type(out, "character")
  expect_length(out, 1L)
})

test_that("build_node_context returns empty string for unknown node", {
  g <- make_ctx_graph()
  expect_warning(
    out <- build_node_context("NOTHERE", g),
    regexp = NULL
  )
  expect_equal(out, "")
})

# ---- build_node_context: full mode ----------------------------------

test_that("full mode contains the function signature", {
  g <- make_ctx_graph()
  out <- build_node_context("pkg::load_data", g, mode = "full")
  expect_match(out, "load_data", fixed = TRUE)
})

test_that("full mode contains the body text", {
  g <- make_ctx_graph()
  out <- build_node_context("pkg::load_data", g, mode = "full")
  expect_match(out, "read.csv", fixed = TRUE)
})

test_that("full mode contains roxygen text when present", {
  g <- make_ctx_graph()
  out <- build_node_context("pkg::load_data", g, mode = "full")
  expect_match(out, "Load a CSV", fixed = TRUE)
})

# ---- build_node_context: compressed mode ----------------------------

test_that("compressed mode is not longer than full mode (compression ratio)", {
  g <- make_ctx_graph()
  full_nc <- nchar(build_node_context("pkg::clean_data", g, mode = "full"))
  comp_nc <- nchar(build_node_context(
    "pkg::clean_data",
    g,
    mode = "compressed"
  ))
  expect_lte(comp_nc, full_nc)
})

test_that("compressed mode contains the signature", {
  g <- make_ctx_graph()
  out <- build_node_context("pkg::clean_data", g, mode = "compressed")
  expect_match(out, "clean_data", fixed = TRUE)
})

test_that("compressed mode contains one-line description from roxygen", {
  g <- make_ctx_graph()
  out <- build_node_context("pkg::clean_data", g, mode = "compressed")
  expect_match(out, "Clean rows", fixed = TRUE)
})

test_that("compressed mode contains 'Calls:' line when outgoing edges exist", {
  g <- make_ctx_graph()
  # fit_model CALLS clean_data; clean_data CALLS load_data
  out <- build_node_context("pkg::clean_data", g, mode = "compressed")
  # Note: calls reference the node it calls (load_data)
  expect_match(out, "Calls:", fixed = TRUE)
})

test_that("compressed mode contains 'Called by:' line when incoming edges exist", {
  g <- make_ctx_graph()
  # load_data is called by clean_data
  out <- build_node_context("pkg::load_data", g, mode = "compressed")
  expect_match(out, "Called by:", fixed = TRUE)
})

# ---- build_node_context: package node always compressed -------------

test_that("package node always uses compressed mode regardless of mode arg", {
  g <- make_ctx_graph()
  full <- build_node_context("dplyr", g, mode = "full")
  comp <- build_node_context("dplyr", g, mode = "compressed")
  # Both should be short (package nodes have no body)
  expect_lte(
    nchar(full),
    nchar(
      build_node_context("pkg::fit_model", g, mode = "full")
    )
  )
  expect_equal(full, comp)
})

# ---- build_node_context: auto mode is full for user fn --------------

test_that("auto mode returns full source for a user function", {
  g <- make_ctx_graph()
  auto <- build_node_context("pkg::load_data", g, mode = "auto")
  full <- build_node_context("pkg::load_data", g, mode = "full")
  expect_equal(auto, full)
})

# ---- assemble_context_string: return type ---------------------------

test_that("assemble_context_string returns a single character", {
  g <- make_ctx_graph()
  out <- assemble_context_string(
    c("pkg::load_data", "pkg::clean_data"),
    g,
    "load data"
  )
  expect_type(out, "character")
  expect_length(out, 1L)
})

test_that("assemble_context_string output is valid UTF-8", {
  g <- make_ctx_graph()
  out <- assemble_context_string(
    c("pkg::load_data", "pkg::clean_data"),
    g,
    "load data"
  )
  expect_true(validUTF8(out))
})

test_that("nchar is stable on repeated calls (deterministic output)", {
  g <- make_ctx_graph()
  hits <- c("pkg::load_data", "pkg::clean_data")
  n1 <- nchar(assemble_context_string(hits, g, "test"))
  n2 <- nchar(assemble_context_string(hits, g, "test"))
  expect_equal(n1, n2)
})

# ---- assemble_context_string: header --------------------------------

test_that("header contains '# rrlm_graph Context'", {
  g <- make_ctx_graph()
  out <- assemble_context_string("pkg::load_data", g, "load")
  expect_match(out, "rrlm_graph Context", fixed = TRUE)
})

test_that("header contains the project name", {
  g <- make_ctx_graph()
  out <- assemble_context_string("pkg::load_data", g, "load")
  expect_match(out, "mypkg", fixed = TRUE)
})

test_that("header contains a non-zero token count", {
  g <- make_ctx_graph()
  out <- assemble_context_string("pkg::load_data", g, "load")
  # Should contain something like "~<N> tokens" where N > 0
  expect_match(out, "tokens", fixed = TRUE)
  toks <- regmatches(out, regexpr("~(\\d+) tokens", out))
  n <- as.integer(gsub("[^0-9]", "", toks))
  expect_gt(n, 0)
})

test_that("header contains the query string", {
  g <- make_ctx_graph()
  out <- assemble_context_string("pkg::load_data", g, "my special query")
  expect_match(out, "my special query", fixed = TRUE)
})

# ---- assemble_context_string: sections ------------------------------

test_that("output always contains CORE FUNCTIONS section", {
  g <- make_ctx_graph()
  out <- assemble_context_string(c("pkg::load_data", "pkg::clean_data"), g)
  expect_match(out, "CORE FUNCTIONS", fixed = TRUE)
})

test_that("output always contains CONSTRAINTS section", {
  g <- make_ctx_graph()
  out <- assemble_context_string("pkg::load_data", g)
  expect_match(out, "CONSTRAINTS", fixed = TRUE)
})

test_that("seed node appears in CORE FUNCTIONS section", {
  g <- make_ctx_graph()
  out <- assemble_context_string(
    c("pkg::load_data", "pkg::clean_data"),
    g
  )
  # Core section appears before Supporting; seed name must be present
  core_start <- regexpr("CORE FUNCTIONS", out)
  supp_start <- regexpr("SUPPORTING FUNCTIONS", out)
  seed_start <- regexpr("load_data", out)

  expect_true(core_start > 0)
  expect_true(seed_start > core_start)
  if (supp_start > 0) expect_true(seed_start < supp_start)
})

test_that("SUPPORTING FUNCTIONS section present when there are non-seed nodes", {
  g <- make_ctx_graph()
  out <- assemble_context_string(
    c("pkg::load_data", "pkg::clean_data", "pkg::fit_model"),
    g
  )
  expect_match(out, "SUPPORTING FUNCTIONS", fixed = TRUE)
})

test_that("FRAMEWORK / PACKAGE CONTEXT section present when pkg nodes in hits", {
  g <- make_ctx_graph()
  out <- assemble_context_string(
    c("pkg::load_data", "dplyr"),
    g
  )
  expect_match(out, "PACKAGE CONTEXT", fixed = TRUE)
})

# ---- assemble_context_string: task history --------------------------

test_that("RECENT TASK HISTORY absent when graph has no task_history attr", {
  g <- make_ctx_graph()
  out <- assemble_context_string("pkg::load_data", g, "test")
  expect_false(grepl("RECENT TASK HISTORY", out, fixed = TRUE))
})

test_that("RECENT TASK HISTORY present when task_history is set", {
  g <- make_ctx_graph()
  igraph::graph_attr(g, "task_history") <- list(
    "Task 1: load data from CSV",
    "Task 2: clean missing values"
  )
  out <- assemble_context_string("pkg::load_data", g, "test")
  expect_match(out, "RECENT TASK HISTORY", fixed = TRUE)
  expect_match(out, "load data from CSV", fixed = TRUE)
})

test_that("RECENT TASK HISTORY shows at most 3 entries", {
  g <- make_ctx_graph()
  igraph::graph_attr(g, "task_history") <- as.list(paste0("Task ", 1:10))
  out <- assemble_context_string("pkg::load_data", g, "test")
  # extract the section and count "N." prefixes
  hist_start <- regexpr("RECENT TASK HISTORY", out)
  constr_start <- regexpr("CONSTRAINTS", out)
  section <- substring(out, hist_start, constr_start - 1L)
  n_entries <- length(gregexpr("[0-9]+\\.", section)[[1L]])
  expect_lte(n_entries, 3L)
})

# ---- assemble_context_string: empty hits ----------------------------

test_that("empty hits returns a non-empty fallback string", {
  g <- make_ctx_graph()
  out <- assemble_context_string(character(0), g, "nothing")
  expect_type(out, "character")
  expect_true(nzchar(out))
  expect_match(out, "CONSTRAINTS", fixed = TRUE)
})
