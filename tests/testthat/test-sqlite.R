# tests/testthat/test-sqlite.R
# Sprint 2 integration tests: export_to_sqlite DBI readability + roundtrip.
# Issue #16 acceptance criteria.

skip_if_not_installed("igraph")
skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

# ---- fixture helpers ------------------------------------------------

make_full_graph <- function(project_root = NULL) {
  verts <- data.frame(
    name = c(
      "pkg::load_data",
      "pkg::clean_data",
      "pkg::fit_model",
      "pkg::score_model"
    ),
    node_type = c("function", "function", "function", "function"),
    pagerank = c(0.15, 0.25, 0.40, 0.20),
    file = c("R/data.R", "R/data.R", "R/model.R", "R/model.R"),
    signature = c(
      "load_data(path)",
      "clean_data(df)",
      "fit_model(df, formula)",
      "score_model(model, df)"
    ),
    complexity = c(1L, 2L, 3L, 2L),
    task_weight = c(0.5, 0.6, 0.8, 0.7),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("pkg::load_data", "pkg::clean_data", "pkg::fit_model"),
    to = c("pkg::clean_data", "pkg::fit_model", "pkg::score_model"),
    weight = c(1.0, 1.0, 0.8),
    edge_type = c("CALLS", "CALLS", "CALLS"),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "fullpkg"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0.1
  if (!is.null(project_root)) {
    igraph::graph_attr(g, "project_root") <- project_root
  }
  class(g) <- c("rrlm_graph", class(g))
  g
}

# Open a DBI connection and run a query helper
db_query <- function(db_path, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql)
}

# ---- DBI readability ------------------------------------------------

test_that("SQLite file is readable by DBI::dbConnect", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()

  export_to_sqlite(g, db)

  expect_no_error({
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    DBI::dbDisconnect(con)
  })
})

test_that("DBI can list all expected tables", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  tables <- DBI::dbListTables(con)
  DBI::dbDisconnect(con)

  # tfidf_vocab added in Phase 2; sqlite_sequence is an internal SQLite table
  # created whenever AUTOINCREMENT is used — check for required tables only
  expect_true(
    all(
      c("nodes", "edges", "task_traces", "graph_metadata", "tfidf_vocab") %in%
        tables
    )
  )
})

test_that("DBI can SELECT * FROM nodes without error", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  expect_no_error({
    rows <- db_query(db, "SELECT * FROM nodes")
  })
  expect_equal(nrow(rows), igraph::vcount(g))
})

test_that("DBI can SELECT * FROM edges without error", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  rows <- db_query(db, "SELECT * FROM edges")
  expect_equal(nrow(rows), igraph::ecount(g))
})

# ---- Schema column presence -----------------------------------------

test_that("nodes table has all required columns", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  rs <- DBI::dbSendQuery(con, "SELECT * FROM nodes LIMIT 1")
  info <- DBI::dbColumnInfo(rs)
  DBI::dbClearResult(rs)
  DBI::dbDisconnect(con)

  required_cols <- c(
    "node_id",
    "name",
    "file",
    "node_type",
    "signature",
    "body_text",
    "pagerank",
    "task_weight",
    "embedding"
  )
  expect_true(all(required_cols %in% info$name))
})

test_that("edges table has all required columns", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  rs <- DBI::dbSendQuery(con, "SELECT * FROM edges LIMIT 1")
  info <- DBI::dbColumnInfo(rs)
  DBI::dbClearResult(rs)
  DBI::dbDisconnect(con)

  expect_true(all(
    c("source_id", "target_id", "edge_type", "weight") %in%
      info$name
  ))
})

# ---- Roundtrip data integrity ----------------------------------------

test_that("node_id values in SQLite match V(graph)$name", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  rows <- db_query(db, "SELECT node_id FROM nodes ORDER BY node_id")
  expect_setequal(rows$node_id, igraph::V(g)$name)
})

test_that("edge source_id and target_id are valid node_ids", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  node_ids <- db_query(db, "SELECT node_id FROM nodes")$node_id
  edges <- db_query(db, "SELECT source_id, target_id FROM edges")

  expect_true(all(edges$source_id %in% node_ids))
  expect_true(all(edges$target_id %in% node_ids))
})

test_that("pagerank values round-trip within numeric tolerance", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  row <- db_query(
    db,
    "SELECT pagerank FROM nodes WHERE node_id = 'pkg::fit_model'"
  )
  expect_equal(row$pagerank, 0.40, tolerance = 1e-6)
})

test_that("edge_type values are preserved", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  types <- db_query(db, "SELECT DISTINCT edge_type FROM edges")$edge_type
  expect_equal(types, "CALLS")
})

# ---- graph_metadata roundtrip ----------------------------------------

test_that("project_name stored in graph_metadata round-trips correctly", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  meta <- db_query(
    db,
    "SELECT value FROM graph_metadata WHERE key = 'project_name'"
  )
  expect_equal(meta$value, "fullpkg")
})

test_that("node_count in graph_metadata equals actual node count", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  meta_n <- as.integer(
    db_query(
      db,
      "SELECT value FROM graph_metadata WHERE key = 'node_count'"
    )$value
  )
  db_count <- db_query(db, "SELECT COUNT(*) AS n FROM nodes")$n

  expect_equal(meta_n, db_count)
})

# ---- SQL query patterns (MCP-style) ----------------------------------

test_that("JOIN nodes+edges WHERE edge_type = 'CALLS' returns expected rows", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  rows <- db_query(
    db,
    "
    SELECT n1.name AS caller, n2.name AS callee
    FROM edges e
    JOIN nodes n1 ON e.source_id = n1.node_id
    JOIN nodes n2 ON e.target_id = n2.node_id
    WHERE e.edge_type = 'CALLS'
  "
  )

  expect_gte(nrow(rows), 1L)
  expect_true(all(c("caller", "callee") %in% names(rows)))
})

test_that("SELECT nodes ORDER BY pagerank DESC returns fit_model first", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  rows <- db_query(
    db,
    "SELECT node_id FROM nodes ORDER BY pagerank DESC LIMIT 1"
  )
  expect_equal(rows$node_id, "pkg::fit_model")
})

test_that("nodes filterable by node_type via SQL", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "out.sqlite")
  g <- make_full_graph()
  export_to_sqlite(g, db)

  fn_nodes <- db_query(
    db,
    "SELECT COUNT(*) AS n FROM nodes WHERE node_type = 'function'"
  )$n
  expect_equal(fn_nodes, igraph::vcount(g))
})

# ---- save + export pipeline integration -----------------------------

test_that("save_graph_cache then export_to_sqlite pipeline works", {
  tmp <- withr::local_tempdir()
  g <- make_full_graph(project_root = tmp)
  db <- file.path(tmp, "graph.sqlite")

  save_graph_cache(g, cache_dir = file.path(tmp, ".rrlmgraph"))
  g2 <- load_graph_cache(tmp)

  expect_no_error(export_to_sqlite(g2, db))

  n <- db_query(db, "SELECT COUNT(*) AS n FROM nodes")$n
  expect_equal(n, igraph::vcount(g))
})
