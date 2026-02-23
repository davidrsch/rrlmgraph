# tests/testthat/test-task-trace.R
# Unit tests for log_task_trace(), update_task_weights(),
# and update_task_polarity().
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

# ---- update_task_polarity early-return paths -------------------------

test_that("update_task_polarity warns when graph has no project_root", {
  g_no_root <- make_tt_graph() # no project_root set
  ctx <- structure(
    list(
      nodes = c("pkg::load_data"),
      context_string = "test",
      tokens_used = 1L,
      budget_tokens = 100L,
      seed_node = "pkg::load_data",
      relevance_scores = c("pkg::load_data" = 1.0)
    ),
    class = c("rrlm_context", "list")
  )
  expect_warning(
    update_task_polarity(g_no_root, context = ctx, polarity = 0.5),
    regexp = "Cannot update polarity"
  )
})

test_that("update_task_polarity returns silently when trace file is absent", {
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)
  # No JSONL file written yet
  ctx <- structure(
    list(
      nodes = c("pkg::load_data"),
      context_string = "test",
      tokens_used = 1L,
      budget_tokens = 100L,
      seed_node = "pkg::load_data",
      relevance_scores = c("pkg::load_data" = 1.0)
    ),
    class = c("rrlm_context", "list")
  )
  result <- update_task_polarity(g, context = ctx, polarity = 0.5)
  expect_s3_class(result, "igraph")
})

test_that("update_task_polarity returns unchanged when no entries match", {
  skip_if_not_installed("jsonlite")
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  log_task_trace("q1", c("pkg::model"), g, polarity = 0)

  ctx <- structure(
    list(
      nodes = c("pkg::nonexistent_node"),
      context_string = "test",
      tokens_used = 1L,
      budget_tokens = 100L,
      seed_node = "pkg::nonexistent_node",
      relevance_scores = c("pkg::nonexistent_node" = 1.0)
    ),
    class = c("rrlm_context", "list")
  )
  update_task_polarity(g, context = ctx, polarity = 0.9)

  path <- file.path(tmp, ".rrlmgraph", "task_trace.jsonl")
  lines <- readLines(path, warn = FALSE)
  entries <- lapply(lines, jsonlite::fromJSON)
  polarities <- vapply(entries, `[[`, numeric(1), "polarity")
  # Original polarity 0 should be unchanged
  expect_equal(polarities, 0)
})

# ---- internal helper coverage ---------------------------------------

test_that(".trace_project_root returns NULL when graph has no project_root", {
  g_no_root <- make_tt_graph()
  result <- rrlmgraph:::.trace_project_root(g_no_root)
  expect_null(result)
})

test_that(".trace_project_root returns NULL when project_root is empty string", {
  g <- make_tt_graph(project_root = "")
  result <- rrlmgraph:::.trace_project_root(g)
  expect_null(result)
})

test_that(".ensure_trace_file returns NULL when project_root is NULL", {
  result <- rrlmgraph:::.ensure_trace_file(NULL)
  expect_null(result)
})

test_that(".trace_project_root returns path when set", {
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)
  result <- rrlmgraph:::.trace_project_root(g)
  expect_equal(result, tmp)
})

# ---- log_task_trace input validation --------------------------------

test_that("log_task_trace errors on non-character query", {
  g <- make_tt_graph()
  expect_error(
    log_task_trace(42L, c("pkg::load_data"), g),
    regexp = "query"
  )
})

test_that("log_task_trace errors on length-2 query", {
  g <- make_tt_graph()
  expect_error(
    log_task_trace(c("a", "b"), c("pkg::load_data"), g),
    regexp = "query"
  )
})

# ---- update_task_weights input validation ---------------------------

test_that("update_task_weights errors on non-igraph input", {
  expect_error(
    update_task_weights(list()),
    regexp = "igraph"
  )
})

test_that("update_task_weights returns graph unchanged for 0-node graph", {
  g_empty <- igraph::make_empty_graph(n = 0, directed = TRUE)
  class(g_empty) <- c("rrlm_graph", class(g_empty))
  result <- update_task_weights(g_empty, useful_nodes = c("pkg::foo"))
  expect_equal(igraph::vcount(result), 0L)
})

test_that("update_task_weights skips malformed JSONL entries", {
  skip_if_not_installed("jsonlite")
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  # Write a valid JSONL entry and a malformed one
  trace_dir <- file.path(tmp, ".rrlmgraph")
  dir.create(trace_dir, recursive = TRUE, showWarnings = FALSE)
  trace_file <- file.path(trace_dir, "task_trace.jsonl")
  writeLines(
    c(
      jsonlite::toJSON(
        list(
          timestamp = "2024-01-01T00:00:00Z",
          query = "q1",
          nodes = list("pkg::load_data"),
          polarity = 0,
          session_id = "s1"
        ),
        auto_unbox = TRUE
      ),
      "not valid json {{{" # malformed — forces the next branch
    ),
    trace_file
  )

  # Should not error despite the malformed line
  expect_no_error(
    update_task_weights(g, useful_nodes = c("pkg::load_data"))
  )
})

# ---- update_task_polarity input validation --------------------------

# Shared context fixture for polarity tests
make_tt_ctx <- function(nodes = c("pkg::load_data")) {
  structure(
    list(
      nodes = nodes,
      context_string = "test",
      tokens_used = 1L,
      budget_tokens = 100L,
      seed_node = nodes[[1L]],
      relevance_scores = setNames(rep(1.0, length(nodes)), nodes)
    ),
    class = c("rrlm_context", "list")
  )
}

test_that("update_task_polarity errors on non-rrlm_context", {
  g <- make_tt_graph()
  expect_error(
    update_task_polarity(g, context = list(nodes = "x"), polarity = 0.5),
    regexp = "rrlm_context"
  )
})

test_that("update_task_polarity errors on polarity out of range", {
  g <- make_tt_graph()
  expect_error(
    update_task_polarity(g, context = make_tt_ctx(), polarity = 1.5),
    regexp = "polarity"
  )
})

test_that("update_task_polarity handles blank and malformed trace lines", {
  skip_if_not_installed("jsonlite")
  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  # Write: blank line + malformed JSON + valid entry
  trace_dir <- file.path(tmp, ".rrlmgraph")
  dir.create(trace_dir, recursive = TRUE, showWarnings = FALSE)
  trace_file <- file.path(trace_dir, "task_trace.jsonl")
  valid_entry <- jsonlite::toJSON(
    list(
      timestamp = "2024-01-01T00:00:00Z",
      query = "q1",
      nodes = list("pkg::load_data"),
      polarity = 0,
      session_id = "s1"
    ),
    auto_unbox = TRUE
  )
  writeLines(
    c("", "not json {{{", as.character(valid_entry)),
    trace_file
  )

  ctx <- make_tt_ctx()
  # Should not error; blank line and malformed line are skipped
  expect_no_error(
    update_task_polarity(g, context = ctx, polarity = 0.8)
  )

  # Valid entry should have polarity updated
  result_lines <- readLines(trace_file, warn = FALSE)
  non_empty <- result_lines[
    nchar(trimws(result_lines)) > 0L &
      !grepl("not json", result_lines)
  ]
  entry <- jsonlite::fromJSON(non_empty[[length(non_empty)]])
  expect_equal(as.numeric(entry$polarity), 0.8)
})

# ---- .tt_session_id -------------------------------------------------

test_that(".tt_session_id returns non-empty string when env var absent", {
  withr::with_envvar(
    c(RRLMGRAPH_SESSION_ID = ""),
    {
      id <- rrlmgraph:::.tt_session_id()
      expect_type(id, "character")
      expect_length(id, 1L)
      expect_gt(nchar(id), 0L)
    }
  )
})

# ---- update_task_weights — SQLite source (issue #67) ----------------

.make_sqlite_with_traces <- function(sqlite_path, rows) {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS task_traces (
       trace_id   INTEGER PRIMARY KEY AUTOINCREMENT,
       query      TEXT,
       nodes_json TEXT,
       polarity   REAL    DEFAULT 0,
       session_id TEXT,
       created_at TEXT
     )"
  )
  for (r in rows) {
    DBI::dbExecute(
      con,
      "INSERT INTO task_traces (query, nodes_json, polarity, session_id, created_at)
       VALUES (?, ?, ?, ?, ?)",
      params = list(
        r$query,
        r$nodes_json,
        r$polarity,
        r$session_id,
        r$created_at
      )
    )
  }
  invisible(sqlite_path)
}

test_that("update_task_weights reads from SQLite when sqlite_path provided", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  sqlite_path <- file.path(tmp, "graph.sqlite")
  .make_sqlite_with_traces(
    sqlite_path,
    list(
      list(
        query = "How does load_data work?",
        nodes_json = jsonlite::toJSON(
          list("pkg::load_data"),
          auto_unbox = FALSE
        ),
        polarity = 1.0,
        session_id = "mcp-s1",
        created_at = "2024-01-01T00:00:00Z"
      )
    )
  )

  g2 <- update_task_weights(g, sqlite_path = sqlite_path)

  w <- stats::setNames(
    igraph::V(g2)$task_trace_weight,
    igraph::V(g2)$name
  )
  # pkg::load_data was the only trace node — it must have the highest weight
  expect_gt(w[["pkg::load_data"]], w[["pkg::clean"]])
  expect_gt(w[["pkg::load_data"]], w[["pkg::model"]])
})

test_that("update_task_weights merges JSONL and SQLite traces without duplicates", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  # Write a JSONL trace for pkg::clean
  trace_dir <- file.path(tmp, ".rrlmgraph")
  dir.create(trace_dir, showWarnings = FALSE, recursive = TRUE)
  trace_file <- file.path(trace_dir, "task_trace.jsonl")
  writeLines(
    as.character(jsonlite::toJSON(
      list(
        timestamp = "2024-01-02T00:00:00Z",
        query = "clean data",
        nodes = list("pkg::clean"),
        polarity = 0.8,
        session_id = "r-s1"
      ),
      auto_unbox = TRUE
    )),
    trace_file
  )

  # SQLite trace for pkg::model (different session + timestamp => not a dup)
  sqlite_path <- file.path(trace_dir, "graph.sqlite")
  .make_sqlite_with_traces(
    sqlite_path,
    list(
      list(
        query = "run model",
        nodes_json = jsonlite::toJSON(list("pkg::model"), auto_unbox = FALSE),
        polarity = 0.9,
        session_id = "mcp-s2",
        created_at = "2024-01-03T00:00:00Z"
      )
    )
  )

  g2 <- update_task_weights(g, sqlite_path = sqlite_path)

  w <- stats::setNames(
    igraph::V(g2)$task_trace_weight,
    igraph::V(g2)$name
  )
  # Both non-load_data nodes should be boosted above 0.1 (the min after normalise)
  expect_gt(w[["pkg::clean"]], 0.1)
  expect_gt(w[["pkg::model"]], 0.1)
})

test_that("update_task_weights skips SQLite when sqlite_path = NA", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()
  g <- make_tt_graph(project_root = tmp)

  # No SQLite, no JSONL — should fall through to EMA path without error
  g2 <- update_task_weights(
    g,
    useful_nodes = c("pkg::load_data"),
    sqlite_path = NA_character_
  )
  expect_s3_class(g2, "igraph")
  # EMA should have boosted pkg::load_data slightly
  w <- stats::setNames(
    igraph::V(g2)$task_trace_weight,
    igraph::V(g2)$name
  )
  expect_gt(w[["pkg::load_data"]], w[["pkg::clean"]])
})

test_that(".read_traces_sqlite returns NULL for missing file", {
  expect_null(rrlmgraph:::.read_traces_sqlite("/nonexistent/path.sqlite"))
  expect_null(rrlmgraph:::.read_traces_sqlite(NULL))
  expect_null(rrlmgraph:::.read_traces_sqlite(""))
})
