# tests/testthat/test-sqlite-export.R
# Unit tests for export_to_sqlite()
# Issue #15 acceptance criteria.

skip_if_not_installed("igraph")
skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

# ---- fixture helpers ------------------------------------------------

make_sqlite_graph <- function() {
  verts <- data.frame(
    name = c("pkg::train", "pkg::evaluate", "pkg::load_data"),
    node_type = c("function", "function", "function"),
    pagerank = c(0.5, 0.3, 0.2),
    file = c("R/model.R", "R/model.R", "R/data.R"),
    signature = c(
      "train(data, epochs)",
      "evaluate(model, test_data)",
      "load_data(path)"
    ),
    complexity = c(3L, 2L, 1L),
    task_weight = c(0.8, 0.5, 0.3),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("pkg::train", "pkg::evaluate"),
    to = c("pkg::evaluate", "pkg::load_data"),
    weight = c(1.0, 0.8),
    edge_type = c("CALLS", "CALLS"),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "sqlpkg"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0.05
  class(g) <- c("rrlm_graph", class(g))
  g
}

make_embedding_graph <- function() {
  verts <- data.frame(
    name = c("pkg::embed_a", "pkg::embed_b"),
    node_type = "function",
    pagerank = c(0.6, 0.4),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = data.frame(
      from = "pkg::embed_a",
      to = "pkg::embed_b",
      weight = 1,
      edge_type = "CALLS"
    ),
    vertices = verts,
    directed = TRUE
  )
  # Assign a fake embedding vector to each node
  igraph::V(g)$embedding <- list(c(0.1, 0.2, 0.3), c(0.4, 0.5, 0.6))
  class(g) <- c("rrlm_graph", class(g))
  g
}

# ---- basic creation -------------------------------------------------

test_that("export_to_sqlite creates the SQLite file", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "test.sqlite")
  g <- make_sqlite_graph()

  expect_no_error(export_to_sqlite(g, db))
  expect_true(file.exists(db))
})

test_that("export_to_sqlite returns db_path invisibly", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "test.sqlite")
  g <- make_sqlite_graph()

  ret <- withVisible(export_to_sqlite(g, db))
  expect_false(ret$visible)
  expect_equal(ret$value, db)
})

# ---- table existence ------------------------------------------------

test_that("all four tables exist after export", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "test.sqlite")
  g <- make_sqlite_graph()
  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  tables <- DBI::dbListTables(con)
  expect_true("nodes" %in% tables)
  expect_true("edges" %in% tables)
  expect_true("task_traces" %in% tables)
  expect_true("graph_metadata" %in% tables)
})

# ---- node count -----------------------------------------------------

test_that("nodes table row count matches vcount(graph)", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "test.sqlite")
  g <- make_sqlite_graph()
  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  n_nodes <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM nodes")$n
  expect_equal(n_nodes, igraph::vcount(g))
})

# ---- edge count -----------------------------------------------------

test_that("edges table row count matches ecount(graph)", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "test.sqlite")
  g <- make_sqlite_graph()
  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  n_edges <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM edges")$n
  expect_equal(n_edges, igraph::ecount(g))
})

# ---- vertex attributes preserved ------------------------------------

test_that("node attributes (pagerank, node_type, file) are stored", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "test.sqlite")
  g <- make_sqlite_graph()
  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  row <- DBI::dbGetQuery(
    con,
    "SELECT * FROM nodes WHERE node_id = 'pkg::train'"
  )

  expect_equal(nrow(row), 1L)
  expect_equal(row$node_type, "function")
  expect_equal(row$file, "R/model.R")
  expect_equal(as.numeric(row$pagerank), 0.5, tolerance = 1e-6)
})

# ---- graph_metadata -------------------------------------------------

test_that("graph_metadata contains project_name and node_count", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "test.sqlite")
  g <- make_sqlite_graph()
  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  meta <- DBI::dbGetQuery(con, "SELECT key, value FROM graph_metadata")
  keys <- meta$key

  expect_true("project_name" %in% keys)
  expect_true("node_count" %in% keys)
  expect_true("edge_count" %in% keys)
})

# ---- idempotency (upsert) -------------------------------------------

test_that("exporting twice does not duplicate rows", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "test.sqlite")
  g <- make_sqlite_graph()

  export_to_sqlite(g, db)
  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  n_nodes <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM nodes")$n
  n_edges <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM edges")$n

  expect_equal(n_nodes, igraph::vcount(g))
  expect_equal(n_edges, igraph::ecount(g))
})

# ---- embedding JSON round-trip --------------------------------------

test_that("embedding stored as JSON array parses back to numeric", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "test.sqlite")
  g <- make_embedding_graph()
  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  row <- DBI::dbGetQuery(
    con,
    "SELECT embedding FROM nodes WHERE node_id = 'pkg::embed_a'"
  )

  expect_false(is.na(row$embedding))
  parsed <- jsonlite::fromJSON(row$embedding)
  expect_equal(as.numeric(parsed), c(0.1, 0.2, 0.3), tolerance = 1e-9)
})

# ---- indexes --------------------------------------------------------

test_that("expected indexes exist", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "test.sqlite")
  g <- make_sqlite_graph()
  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  idx_info <- DBI::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'index' AND name LIKE 'idx_%'"
  )
  idx_names <- idx_info$name

  expect_true(any(grepl("source", idx_names, ignore.case = TRUE)))
  expect_true(any(grepl("target", idx_names, ignore.case = TRUE)))
})

# ---- input validation -----------------------------------------------

test_that("export_to_sqlite errors on non-igraph input", {
  tmp <- withr::local_tempdir()
  expect_error(
    export_to_sqlite(list(), file.path(tmp, "x.sqlite")),
    regexp = "igraph"
  )
})

test_that("export_to_sqlite errors on non-character db_path", {
  g <- make_sqlite_graph()
  expect_error(export_to_sqlite(g, 123L), regexp = "db_path")
})

# ---- task trace import ----------------------------------------------

test_that("task_traces table populated from task_trace.jsonl", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "test.sqlite")
  g <- make_sqlite_graph()
  igraph::graph_attr(g, "project_root") <- tmp

  trace_dir <- file.path(tmp, ".rrlmgraph")
  dir.create(trace_dir, recursive = TRUE)
  trace_file <- file.path(trace_dir, "task_trace.jsonl")
  entry <- jsonlite::toJSON(
    list(
      timestamp = "2026-02-19T22:00:00Z",
      query = "train the model",
      nodes = list("pkg::train"),
      response = "Response text",
      session_id = "sess-001"
    ),
    auto_unbox = TRUE
  )
  cat(entry, "\n", file = trace_file, sep = "")

  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  n_traces <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM task_traces")$n
  expect_gte(n_traces, 1L)

  row <- DBI::dbGetQuery(con, "SELECT query FROM task_traces LIMIT 1")
  expect_equal(row$query, "train the model")
})
