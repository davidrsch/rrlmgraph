# tests/testthat/test-cache-extra.R
# Branch-coverage tests for cache.R helpers that are not reached by the
# primary test files (test-cache.R, test-sqlite-export.R, test-sqlite.R).

skip_if_not_installed("igraph")
skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

# ---- fixture helpers ------------------------------------------------

make_extra_cache_graph <- function(project_root = NULL) {
  verts <- data.frame(
    name = c("pkg::alpha", "pkg::beta"),
    node_type = "function",
    pagerank = c(0.6, 0.4),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = "pkg::alpha",
    to = "pkg::beta",
    weight = 1.0,
    edge_type = "CALLS",
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "extrapkg"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  igraph::graph_attr(g, "build_time") <- 0.01
  if (!is.null(project_root)) {
    igraph::graph_attr(g, "project_root") <- project_root
  }
  class(g) <- c("rrlm_graph", class(g))
  g
}

# Helper: export a graph with the given embed_model, return tfidf_vocab rows.
# Manages its own temp directory + DB connection lifecycle.
tfidf_vocab_rows <- function(mock_model) {
  tmp <- tempfile()
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  db <- file.path(tmp, "tfidf.sqlite")
  g <- make_extra_cache_graph()
  igraph::graph_attr(g, "embed_model") <- mock_model
  export_to_sqlite(g, db)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "SELECT * FROM tfidf_vocab")
}

# ---- export_to_sqlite: creates nested db directory ------------------

test_that("export_to_sqlite creates nested db directory when it does not exist", {
  tmp <- withr::local_tempdir()
  # db is nested in a subdir that doesn't exist yet
  db <- file.path(tmp, "nested", "subdir", "test.sqlite")
  g <- make_extra_cache_graph()

  expect_no_error(export_to_sqlite(g, db))
  expect_true(file.exists(db))
})

# ---- .upsert_nodes: 0-vertex graph ----------------------------------

test_that("export_to_sqlite handles 0-vertex graph (empty upsert_nodes path)", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "empty.sqlite")

  # Create an rrlm_graph with no vertices
  g_empty <- igraph::make_empty_graph(n = 0, directed = TRUE)
  igraph::graph_attr(g_empty, "project_name") <- "emptypkg"
  igraph::graph_attr(g_empty, "embed_method") <- "tfidf"
  class(g_empty) <- c("rrlm_graph", class(g_empty))

  expect_no_error(export_to_sqlite(g_empty, db))

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM nodes")$n, 0L)
})

# ---- .upsert_edges: extra edge attributes (metadata_json path) ------

test_that("export_to_sqlite serialises extra edge attributes to metadata JSON", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "meta.sqlite")

  verts <- data.frame(
    name = c("pkg::a", "pkg::b"),
    node_type = "function",
    pagerank = c(0.5, 0.5),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = "pkg::a",
    to = "pkg::b",
    weight = 1.0,
    edge_type = "CALLS",
    color = "red", # extra attribute — triggers metadata_json branch
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = verts,
    directed = TRUE
  )
  igraph::graph_attr(g, "project_name") <- "metapkg"
  igraph::graph_attr(g, "embed_method") <- "tfidf"
  class(g) <- c("rrlm_graph", class(g))

  expect_no_error(export_to_sqlite(g, db))

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- DBI::dbGetQuery(con, "SELECT metadata FROM edges LIMIT 1")
  # metadata should be a JSON string containing "color"
  expect_false(is.na(row$metadata))
  expect_match(row$metadata, "color", fixed = TRUE)
})

# ---- .upsert_tfidf_vocab: happy path --------------------------------

test_that("export_to_sqlite populates tfidf_vocab when embed_model is present", {
  rows <- tfidf_vocab_rows(list(
    vocab = data.frame(
      term = c("function", "data", "return"),
      doc_count = c(3L, 2L, 1L),
      term_count = c(5L, 3L, 1L),
      stringsAsFactors = FALSE
    ),
    tfidf = list(idf_vector = c(1.5, 1.2, 2.0)),
    vectorizer = NULL,
    embeddings = list()
  ))
  expect_equal(nrow(rows), 3L)
  expect_true(all(c("term", "idf") %in% names(rows)))
})

# ---- .upsert_tfidf_vocab: fallback IDF (no idf_vector) --------------

test_that("export_to_sqlite computes IDF fallback when tfidf object is NULL", {
  # tfidf = NULL → as.numeric(NULL$idf_vector) = numeric(0) → length-mismatch
  # handler fills idf with NA; rows still exist.
  rows <- tfidf_vocab_rows(list(
    vocab = data.frame(
      term = c("alpha", "beta"),
      doc_count = c(2L, 1L),
      term_count = c(4L, 1L),
      stringsAsFactors = FALSE
    ),
    tfidf = NULL,
    vectorizer = NULL,
    embeddings = list()
  ))
  expect_equal(nrow(rows), 2L)
  expect_equal(rows$term, c("alpha", "beta"))
})

test_that("export_to_sqlite uses error-fallback IDF when tfidf$idf_vector access errors", {
  # A non-list tfidf causes `"string"$idf_vector` to throw →
  # tryCatch activates fallback log-IDF (non-NA values).
  rows <- tfidf_vocab_rows(list(
    vocab = data.frame(
      term = c("go", "code"),
      doc_count = c(3L, 2L),
      term_count = c(5L, 2L),
      stringsAsFactors = FALSE
    ),
    tfidf = "string_triggers_dollar_error",
    vectorizer = NULL,
    embeddings = list()
  ))
  expect_equal(nrow(rows), 2L)
  expect_true(all(!is.na(rows$idf)))
})

# ---- .upsert_tfidf_vocab: length-mismatched idf_vector (truncate / pad) ------

test_that("export_to_sqlite truncates idf_vector longer than vocab", {
  # 5 idf values, 3-term vocab → truncated to 3
  rows <- tfidf_vocab_rows(list(
    vocab = data.frame(
      term = c("a", "b", "c"),
      doc_count = c(1L, 1L, 1L),
      term_count = c(1L, 1L, 1L),
      stringsAsFactors = FALSE
    ),
    tfidf = list(idf_vector = c(1.1, 1.2, 1.3, 1.4, 1.5)),
    vectorizer = NULL,
    embeddings = list()
  ))
  expect_equal(nrow(rows), 3L)
})

test_that("export_to_sqlite pads idf_vector shorter than vocab with NA", {
  # 1 idf value, 3-term vocab → padded to 3 rows
  rows <- tfidf_vocab_rows(list(
    vocab = data.frame(
      term = c("x", "y", "z"),
      doc_count = c(3L, 2L, 1L),
      term_count = c(6L, 3L, 1L),
      stringsAsFactors = FALSE
    ),
    tfidf = list(idf_vector = c(2.0)),
    vectorizer = NULL,
    embeddings = list()
  ))
  expect_equal(nrow(rows), 3L)
})

# ---- .upsert_tfidf_vocab: skipped when not a list -------------------

test_that("export_to_sqlite skips tfidf_vocab when embed_model is not a list", {
  rows <- tfidf_vocab_rows("not_a_list")
  expect_equal(nrow(rows), 0L)
})

# ---- .validate_cache_version: version mismatch warning --------------

test_that("load_graph_cache warns on version mismatch", {
  tmp <- withr::local_tempdir()
  g <- make_extra_cache_graph(project_root = tmp)

  cache_dir <- file.path(tmp, ".rrlmgraph")
  save_graph_cache(g, cache_dir = cache_dir)

  # Overwrite config.yml with a fake old version
  yml_path <- file.path(cache_dir, "config.yml")
  yml_lines <- readLines(yml_path, warn = FALSE)
  yml_lines[grepl("^package_version:", yml_lines)] <- 'package_version: "0.0.1"'
  writeLines(yml_lines, yml_path)

  # load_graph_cache calls .validate_cache_version → version mismatch → warn
  expect_warning(
    load_graph_cache(tmp),
    regexp = "0\\.0\\.1"
  )
})

# ---- load_graph_cache: cached object is not igraph ------------------

test_that("load_graph_cache errors when cached RDS is not an igraph", {
  tmp <- withr::local_tempdir()
  cache_dir <- file.path(tmp, ".rrlmgraph")
  dir.create(cache_dir, recursive = TRUE)

  # Save a non-igraph object as graph.rds
  saveRDS(list(a = 1, b = 2), file.path(cache_dir, "graph.rds"))

  expect_error(
    load_graph_cache(tmp),
    regexp = "igraph"
  )
})

# ---- .resolve_cache_dir: fallback to getwd() ------------------------

test_that("save_graph_cache uses getwd() when no cache_dir and no project_root", {
  tmp <- withr::local_tempdir()
  old_wd <- setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  g <- make_extra_cache_graph() # no project_root attribute

  # No cache_dir argument → falls back to getwd()/.rrlmgraph
  expect_no_error(save_graph_cache(g))
  expect_true(file.exists(file.path(tmp, ".rrlmgraph", "graph.rds")))
})

# ---- export_to_sqlite imports task traces when project_root is set --

test_that("export_to_sqlite imports task traces when project_root is set", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "trace.sqlite")
  g <- make_extra_cache_graph(project_root = tmp)

  # Write a minimal task trace
  trace_dir <- file.path(tmp, ".rrlmgraph")
  dir.create(trace_dir, recursive = TRUE, showWarnings = FALSE)
  trace_file <- file.path(trace_dir, "task_trace.jsonl")
  writeLines(
    jsonlite::toJSON(
      list(
        timestamp = "2024-06-01T12:00:00Z",
        query = "how does alpha work?",
        nodes = list("pkg::alpha"),
        polarity = 0.5,
        session_id = "test_session"
      ),
      auto_unbox = TRUE
    ),
    trace_file
  )

  expect_no_error(export_to_sqlite(g, db))

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  traces <- DBI::dbGetQuery(con, "SELECT * FROM task_traces")
  expect_gte(nrow(traces), 1L)
  expect_true(any(grepl("alpha", traces$query)))
})
