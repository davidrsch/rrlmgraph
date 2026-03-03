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
      polarity   REAL DEFAULT 1.0,
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
    task_weight = as.numeric(pull("task_weight", NA_real_)),
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

#' @keywords internal
.upsert_tfidf_vocab <- function(con, graph) {
  model <- tryCatch(
    igraph::graph_attr(graph, "embed_model"),
    error = function(e) NULL
  )
  if (is.null(model) || !is.list(model)) {
    return(invisible(NULL))
  }

  vocab <- model$vocab
  if (is.null(vocab) || !is.data.frame(vocab)) {
    return(invisible(NULL))
  }
  if (!all(c("term", "doc_count", "term_count") %in% names(vocab))) {
    return(invisible(NULL))
  }

  # Extract IDF weights.  .fit_tfidf() now stores them as a plain numeric
  # vector in model$idf_weights (extracted from text2vec's private$idf
  # diagonal matrix while still in scope).  Fall back to the old
  # model$tfidf$idf_vector path for graphs built with older versions of the
  # package, and then to a document-frequency-based estimate.
  idf_weights <- if (
    !is.null(model$idf_weights) &&
      length(model$idf_weights) > 0L &&
      is.numeric(model$idf_weights)
  ) {
    model$idf_weights
  } else {
    tryCatch(
      as.numeric(model$tfidf$idf_vector),
      error = function(e) NULL
    )
  }

  # Final fallback: estimate from document frequencies stored in vocab.
  # IDF(t) = log(1 + N / df(t)); use max(doc_count) as proxy for N.
  if (is.null(idf_weights) || length(idf_weights) == 0L) {
    n_docs <- max(as.integer(vocab$doc_count), na.rm = TRUE)
    idf_weights <- log(1 + n_docs / pmax(as.integer(vocab$doc_count), 1L))
  }

  n_terms <- nrow(vocab)
  if (length(idf_weights) != n_terms) {
    # Truncate or pad to match vocab size
    if (length(idf_weights) > n_terms) {
      idf_weights <- idf_weights[seq_len(n_terms)]
    } else {
      idf_weights <- c(
        idf_weights,
        rep(NA_real_, n_terms - length(idf_weights))
      )
    }
  }

  rows <- data.frame(
    term = as.character(vocab$term),
    idf = as.numeric(idf_weights),
    doc_count = as.integer(vocab$doc_count),
    term_count = as.integer(vocab$term_count),
    stringsAsFactors = FALSE
  )

  DBI::dbExecute(con, "DELETE FROM tfidf_vocab")
  DBI::dbWriteTable(con, "tfidf_vocab", rows, append = TRUE, row.names = FALSE)
  invisible(NULL)
}

#' @keywords internal
.upsert_graph_metadata <- function(con, graph) {
  meta <- .graph_metadata_list(graph)
  keys <- names(meta)
  values <- vapply(meta, as.character, character(1L))

  DBI::dbExecute(con, "DELETE FROM graph_metadata")
  DBI::dbWriteTable(
    con,
    "graph_metadata",
    data.frame(key = keys, value = values, stringsAsFactors = FALSE),
    append = TRUE,
    row.names = FALSE
  )
}

#' @keywords internal
.import_task_traces <- function(con, trace_file) {
  if (!file.exists(trace_file)) {
    return(invisible(NULL))
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(invisible(NULL))
  }

  lines <- readLines(trace_file, warn = FALSE)
  lines <- lines[nchar(trimws(lines)) > 0L]
  if (length(lines) == 0L) {
    return(invisible(NULL))
  }

  rows <- lapply(lines, function(line) {
    tryCatch(
      {
        entry <- jsonlite::fromJSON(line, simplifyVector = TRUE)
        data.frame(
          query = as.character(entry$query %||% NA_character_),
          nodes_json = if (is.null(entry$nodes)) {
            NA_character_
          } else {
            # as.character() strips the S3 json class so rbind() works on R 4.2
            as.character(jsonlite::toJSON(entry$nodes, auto_unbox = FALSE))
          },
          polarity = as.numeric(entry$polarity %||% 1.0),
          session_id = as.character(entry$session_id %||% NA_character_),
          created_at = as.character(entry$timestamp %||% NA_character_),
          stringsAsFactors = FALSE
        )
      },
      error = function(e) NULL
    )
  })

  rows <- do.call(rbind, Filter(Negate(is.null), rows))
  if (!is.null(rows) && nrow(rows) > 0L) {
    # INSERT OR IGNORE respects the unique index on (query, session_id, created_at)
    # so repeated calls to export_to_sqlite() never create duplicate trace rows.
    sql <- paste0(
      "INSERT OR IGNORE INTO task_traces ",
      "(query, nodes_json, polarity, session_id, created_at) ",
      "VALUES (?, ?, ?, ?, ?)"
    )
    for (i in seq_len(nrow(rows))) {
      tryCatch(
        DBI::dbExecute(
          con,
          sql,
          params = list(
            rows$query[[i]],
            as.character(rows$nodes_json[[i]]),
            as.numeric(rows$polarity[[i]]),
            rows$session_id[[i]],
            rows$created_at[[i]]
          )
        ),
        error = function(e) NULL
      )
    }
  }
  invisible(NULL)
}

# ---- Shared helpers --------------------------------------------------

#' @keywords internal
.resolve_cache_dir <- function(graph, cache_dir) {
  if (!is.null(cache_dir)) {
    return(cache_dir)
  }

  project_root <- tryCatch(
    igraph::graph_attr(graph, "project_root"),
    error = function(e) NULL
  )
  if (
    !is.null(project_root) &&
      !is.na(project_root) &&
      nchar(trimws(project_root)) > 0L
  ) {
    return(file.path(project_root, ".rrlmgraph"))
  }
  file.path(getwd(), ".rrlmgraph")
}

#' @keywords internal
.graph_metadata_list <- function(graph) {
  attrs <- igraph::graph_attr(graph)
  meta <- list(
    package_version = as.character(
      utils::packageVersion("rrlmgraph")
    ),
    node_count = as.character(igraph::vcount(graph)),
    edge_count = as.character(igraph::ecount(graph)),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  string_attrs <- Filter(
    function(v) {
      is.character(v) || is.numeric(v) || is.logical(v)
    },
    attrs
  )
  for (nm in names(string_attrs)) {
    meta[[nm]] <- as.character(string_attrs[[nm]])
  }
  meta
}

#' @keywords internal
.write_yaml_lines <- function(lst) {
  vapply(
    names(lst),
    function(k) {
      v <- lst[[k]]
      v_str <- if (is.null(v) || (length(v) == 1L && is.na(v))) {
        "~"
      } else {
        paste0('"', gsub('"', '\\"', as.character(v[[1L]]), fixed = TRUE), '"')
      }
      paste0(k, ": ", v_str)
    },
    character(1L)
  )
}

#' @keywords internal
.validate_cache_version <- function(cache_dir) {
  yml_path <- file.path(cache_dir, "config.yml")
  if (!file.exists(yml_path)) {
    return(invisible(NULL))
  }

  lines <- readLines(yml_path, warn = FALSE)
  ver_line <- grep("^package_version:", lines, value = TRUE)
  if (length(ver_line) == 0L) {
    return(invisible(NULL))
  }

  cached_ver <- trimws(sub(
    "^package_version:\\s*\"?([^\"]+)\"?.*$",
    "\\1",
    ver_line[[1L]]
  ))
  current_ver <- as.character(utils::packageVersion("rrlmgraph"))

  if (!identical(cached_ver, current_ver)) {
    cli::cli_warn(c(
      "Cache was built with rrlmgraph {cached_ver} ",
      "(current: {current_ver}). ",
      "Consider rebuilding with {.fn build_rrlm_graph}."
    ))
  }
  invisible(NULL)
}
