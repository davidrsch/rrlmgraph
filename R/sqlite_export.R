# ---- export_to_sqlite ------------------------------------------------

#' Export an rrlm_graph to a SQLite database
#'
#' Writes nodes, edges, graph metadata, task traces, and the TF-IDF vocabulary
#' to a SQLite file readable directly by \pkg{better-sqlite3} in the TypeScript
#' MCP server.  All operations are idempotent: nodes/edges/metadata are
#' replaced on each call; task-trace rows are inserted with
#' \code{INSERT OR IGNORE} (deduplication on \code{query}, \code{session_id},
#' \code{created_at}).
#'
#' @section Schema:
#' \describe{
#'   \item{\code{nodes}}{node_id (PK), name, file, node_type, signature,
#'     body_text, roxygen_text, complexity, pagerank, task_weight,
#'     embedding (JSON text array of floats), pkg_name, pkg_version}
#'   \item{\code{edges}}{edge_id (PK AUTOINCREMENT), source_id, target_id,
#'     edge_type, weight, metadata (JSON)}
#'   \item{\code{task_traces}}{trace_id (PK AUTOINCREMENT), query,
#'     nodes_json, polarity, session_id, created_at.  Unique on
#'     (query, session_id, created_at) to prevent duplicate imports.}
#'   \item{\code{tfidf_vocab}}{term (PK), idf, doc_count, term_count.
#'     Only populated when \code{embed_method = "tfidf"}.  Allows the
#'     TypeScript MCP server to encode queries in the same vector space
#'     without calling back into R.}
#'   \item{\code{graph_metadata}}{key (PK), value}
#' }
#'
#' @section Embedding format:
#' The \code{embedding} column in \code{nodes} is stored as a JSON text
#' array of floating-point numbers (e.g. \code{[0.12, -0.34, ...]}).
#' In TypeScript, decode with \code{JSON.parse(row.embedding || "[]")}.
#'
#' @param graph An \code{rrlm_graph} / \code{igraph} object.
#' @param db_path Character(1).  Path to the \file{.sqlite} file to
#'   create or update.
#'
#' @return \code{db_path}, invisibly.
#' @seealso [save_graph_cache()]
#' @export
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' export_to_sqlite(g, "mypkg.sqlite")
#' }
export_to_sqlite <- function(graph, db_path) {
  if (!inherits(graph, "igraph")) {
    cli::cli_abort("{.arg graph} must be an igraph / rrlm_graph object.")
  }
  if (!is.character(db_path) || length(db_path) != 1L) {
    cli::cli_abort("{.arg db_path} must be a single file path string.")
  }

  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # ---- create tables --------------------------------------------------
  .create_sqlite_schema(con)

  # ---- nodes ----------------------------------------------------------
  .upsert_nodes(con, graph)

  # ---- edges ----------------------------------------------------------
  .upsert_edges(con, graph)

  # ---- graph_metadata -------------------------------------------------
  .upsert_graph_metadata(con, graph)

  # ---- task_traces ----------------------------------------------------
  project_root <- tryCatch(
    igraph::graph_attr(graph, "project_root"),
    error = function(e) NULL
  )
  if (
    !is.null(project_root) &&
      !is.na(project_root) &&
      nchar(trimws(project_root)) > 0L
  ) {
    trace_file <- file.path(project_root, ".rrlmgraph", "task_trace.jsonl")
    .import_task_traces(con, trace_file)
  }

  # ---- tfidf_vocab --------------------------------------------------
  .upsert_tfidf_vocab(con, graph)

  cli::cli_inform("Graph exported to {.path {db_path}}")
  invisible(db_path)
}

# ---- SQLite schema helpers -------------------------------------------

#' @keywords internal
.create_sqlite_schema <- function(con) {
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS nodes (
      node_id      TEXT PRIMARY KEY,
      name         TEXT,
      file         TEXT,
      node_type    TEXT,
      signature    TEXT,
      body_text    TEXT,
      roxygen_text TEXT,
      complexity   REAL,
      pagerank     REAL,
      task_weight  REAL,
      embedding    TEXT,
      pkg_name     TEXT,
      pkg_version  TEXT
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS edges (
      edge_id    INTEGER PRIMARY KEY AUTOINCREMENT,
      source_id  TEXT NOT NULL,
      target_id  TEXT NOT NULL,
      edge_type  TEXT,
      weight     REAL,
      metadata   TEXT,
      UNIQUE(source_id, target_id, edge_type)
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS task_traces (
      trace_id   INTEGER PRIMARY KEY AUTOINCREMENT,
      query      TEXT,
      nodes_json TEXT,
      polarity   REAL DEFAULT 0.0,
      session_id TEXT,
      created_at TEXT
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS graph_metadata (
      key   TEXT PRIMARY KEY,
      value TEXT
    )
  "
  )

  # Indexes for MCP query patterns
  DBI::dbExecute(
    con,
    "CREATE INDEX IF NOT EXISTS idx_edges_source ON edges(source_id)"
  )
  DBI::dbExecute(
    con,
    "CREATE INDEX IF NOT EXISTS idx_edges_target ON edges(target_id)"
  )
  DBI::dbExecute(
    con,
    "CREATE INDEX IF NOT EXISTS idx_edges_type   ON edges(edge_type)"
  )
  DBI::dbExecute(
    con,
    "CREATE INDEX IF NOT EXISTS idx_nodes_name   ON nodes(name)"
  )

  # TF-IDF vocabulary (used by TypeScript MCP server to encode queries)
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS tfidf_vocab (
      term        TEXT PRIMARY KEY,
      idf         REAL,
      doc_count   INTEGER,
      term_count  INTEGER
    )
  "
  )

  # Unique index on task_traces prevents duplicate rows when export_to_sqlite()
  # is called repeatedly (JSONL → SQLite migration is idempotent).
  DBI::dbExecute(
    con,
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_trace_dedup
       ON task_traces(query, session_id, created_at)"
  )
}

#' @keywords internal
.upsert_nodes <- function(con, graph) {
  vdf <- igraph::as_data_frame(graph, what = "vertices")

  if (nrow(vdf) == 0L) {
    return(invisible(NULL))
  }

  # Helper to pull a column if present, else NA
  pull <- function(col, default = NA_character_) {
    if (col %in% names(vdf)) vdf[[col]] else rep(default, nrow(vdf))
  }

  # Serialize embedding matrix/list column to JSON per row
  emb_raw <- pull("embedding")
  embedding_json <- vapply(
    seq_len(nrow(vdf)),
    function(i) {
      ev <- tryCatch(emb_raw[[i]], error = function(e) NULL)
      if (is.null(ev) || (length(ev) == 1L && is.na(ev))) {
        return(NA_character_)
      }
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        jsonlite::toJSON(as.numeric(ev), auto_unbox = FALSE)
      } else {
        paste0("[", paste(as.numeric(ev), collapse = ","), "]")
      }
    },
    character(1L)
  )

  node_name <- if ("name" %in% names(vdf)) vdf$name else rownames(vdf)

  rows <- data.frame(
    node_id = node_name,
    name = node_name,
    file = pull("file"),
    node_type = pull("node_type"),
    signature = pull("signature"),
    # audit/expert-review fix: correct vertex attribute names.
    # R igraph vertex attrs are "body_text" and "roxygen_text", not "body"/"roxygen".
    body_text = pull("body_text"),
    roxygen_text = pull("roxygen_text"),
    complexity = as.numeric(pull("complexity", NA_real_)),
    pagerank = as.numeric(pull("pagerank", NA_real_)),
    task_weight = as.numeric(pull("task_trace_weight", NA_real_)),
    embedding = embedding_json,
    pkg_name = pull("pkg_name"),
    pkg_version = pull("pkg_version"),
    stringsAsFactors = FALSE
  )

  DBI::dbExecute(con, "DELETE FROM nodes")
  DBI::dbWriteTable(con, "nodes", rows, append = TRUE, row.names = FALSE)
}

#' @keywords internal
.upsert_edges <- function(con, graph) {
  edf <- igraph::as_data_frame(graph, what = "edges")

  if (nrow(edf) == 0L) {
    return(invisible(NULL))
  }

  pull <- function(col, default = NA_character_) {
    if (col %in% names(edf)) edf[[col]] else rep(default, nrow(edf))
  }

  # Build metadata JSON from any extra edge attributes
  known_cols <- c("from", "to", "weight", "edge_type")
  extra_cols <- setdiff(names(edf), known_cols)

  metadata_json <- if (
    length(extra_cols) > 0L && requireNamespace("jsonlite", quietly = TRUE)
  ) {
    vapply(
      seq_len(nrow(edf)),
      function(i) {
        extra <- lapply(extra_cols, function(col) edf[[col]][[i]])
        names(extra) <- extra_cols
        jsonlite::toJSON(extra, auto_unbox = TRUE)
      },
      character(1L)
    )
  } else {
    rep(NA_character_, nrow(edf))
  }

  rows <- data.frame(
    source_id = edf$from,
    target_id = edf$to,
    edge_type = pull("edge_type"),
    weight = as.numeric(pull("weight", NA_real_)),
    metadata = metadata_json,
    stringsAsFactors = FALSE
  )

  DBI::dbExecute(con, "DELETE FROM edges")
  DBI::dbWriteTable(con, "edges", rows, append = TRUE, row.names = FALSE)
}
