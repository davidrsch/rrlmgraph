# R/cache.R
# Graph caching and SQLite export for rrlmgraph.
# Covers rrlmgraph issues #14 (cache) and #15 (SQLite bridge).

# ---- save_graph_cache ------------------------------------------------

#' Save an rrlm_graph to a local cache directory
#'
#' Serialises the graph to \file{.rrlmgraph/graph.rds} and writes a
#' human-readable \file{.rrlmgraph/config.yml} alongside a
#' \file{.rrlmgraph/.gitignore} that keeps the RDS out of version
#' control while tracking \file{config.yml} and
#' \file{task_trace.jsonl}.
#'
#' @param graph An \code{rrlm_graph} / \code{igraph} object.
#' @param cache_dir Character(1) or \code{NULL}.  Path to the cache
#'   directory.  Defaults to \code{.rrlmgraph/} inside the
#'   \code{"project_root"} graph attribute, or the current working
#'   directory when the attribute is absent.
#'
#' @return \code{cache_dir}, invisibly.
#' @seealso [load_graph_cache()], [is_cache_stale()]
#' @export
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' save_graph_cache(g)
#' }
save_graph_cache <- function(graph, cache_dir = NULL) {
  if (!inherits(graph, "igraph")) {
    cli::cli_abort("{.arg graph} must be an igraph / rrlm_graph object.")
  }

  cache_dir <- .resolve_cache_dir(graph, cache_dir)
  fs::dir_create(cache_dir)

  # --- graph.rds -------------------------------------------------------
  rds_path <- fs::path(cache_dir, "graph.rds")
  saveRDS(graph, file = rds_path, compress = "xz")

  # --- config.yml ------------------------------------------------------
  yml_path <- fs::path(cache_dir, "config.yml")
  meta <- .graph_metadata_list(graph)
  yml_lines <- .write_yaml_lines(meta)
  writeLines(yml_lines, yml_path)

  # --- .gitignore -------------------------------------------------------
  gi_path <- fs::path(cache_dir, ".gitignore")
  if (!fs::file_exists(gi_path)) {
    writeLines(
      c(
        "# rrlmgraph cache — generated automatically",
        "*.rds",
        "!config.yml",
        "!task_trace.jsonl"
      ),
      gi_path
    )
  }

  cli::cli_inform("Graph cached at {.path {cache_dir}}")
  invisible(cache_dir)
}

# ---- load_graph_cache ------------------------------------------------

#' Load an rrlm_graph from a local cache
#'
#' Reads \file{.rrlmgraph/graph.rds} from \code{project_path}, validates
#' compatibility with the installed rrlmgraph version, refreshes task-trace
#' weights via \code{update_task_weights()}, and returns the graph.
#'
#' @param project_path Character(1).  Root directory of the R project
#'   (parent of \file{.rrlmgraph/}).
#'
#' @return An \code{rrlm_graph} object, or \code{NULL} invisibly when no
#'   cache exists.
#' @seealso [save_graph_cache()], [is_cache_stale()]
#' @export
#' @examples
#' \dontrun{
#' g <- load_graph_cache("mypkg")
#' }
load_graph_cache <- function(project_path = ".") {
  cache_dir <- file.path(project_path, ".rrlmgraph")
  rds_path <- file.path(cache_dir, "graph.rds")

  if (!file.exists(rds_path)) {
    cli::cli_warn("No cache found at {.path {rds_path}}.")
    return(invisible(NULL))
  }

  graph <- tryCatch(
    readRDS(rds_path),
    error = function(e) {
      cli::cli_abort("Failed to load cache: {conditionMessage(e)}")
    }
  )

  if (!inherits(graph, "igraph")) {
    cli::cli_abort(
      "Cached object at {.path {rds_path}} is not an igraph object."
    )
  }

  # Validate version compatibility
  .validate_cache_version(cache_dir)

  # Refresh task-trace weights
  graph <- tryCatch(
    update_task_weights(graph, useful_nodes = character(0)),
    error = function(e) graph
  )

  cli::cli_inform("Graph loaded from {.path {cache_dir}}")
  graph
}

# ---- is_cache_stale --------------------------------------------------

#' Check whether the graph cache is stale
#'
#' Compares the modification time of the cached \file{graph.rds} against
#' the modification times of all \file{.R} files under
#' \code{project_path}.  Returns \code{TRUE} if any R file is newer than
#' the cache.
#'
#' @param project_path Character(1).  Root directory of the R project.
#'
#' @return Logical(1): \code{TRUE} if the cache is stale or absent.
#' @seealso [save_graph_cache()], [load_graph_cache()]
#' @export
#' @examples
#' \dontrun{
#' if (is_cache_stale("mypkg")) {
#'   g <- build_rrlm_graph("mypkg")
#'   save_graph_cache(g)
#' }
#' }
is_cache_stale <- function(project_path = ".") {
  rds_path <- file.path(project_path, ".rrlmgraph", "graph.rds")

  if (!file.exists(rds_path)) {
    return(TRUE)
  }

  cache_mtime <- file.info(rds_path)$mtime

  r_files <- tryCatch(
    fs::dir_ls(project_path, regexp = "\\.R$", recurse = TRUE),
    error = function(e) character(0)
  )

  if (length(r_files) == 0L) {
    return(FALSE)
  }

  r_mtimes <- file.info(as.character(r_files))$mtime
  any(!is.na(r_mtimes) & r_mtimes > cache_mtime)
}

# ---- export_to_sqlite ------------------------------------------------

#' Export an rrlm_graph to a SQLite database
#'
#' Writes nodes, edges, graph metadata, and task traces to a SQLite file
#' readable directly by \pkg{better-sqlite3} in the TypeScript MCP server.
#' All operations are idempotent (upsert via \code{INSERT OR REPLACE}).
#'
#' @section Schema:
#' \describe{
#'   \item{\code{nodes}}{node_id (PK), name, file, node_type, signature,
#'     body_text, roxygen_text, complexity, pagerank, task_weight,
#'     embedding (JSON), pkg_name, pkg_version}
#'   \item{\code{edges}}{edge_id (PK AUTOINCREMENT), source_id, target_id,
#'     edge_type, weight, metadata (JSON)}
#'   \item{\code{task_traces}}{trace_id (PK AUTOINCREMENT), query,
#'     nodes_json, polarity, session_id, created_at}
#'   \item{\code{graph_metadata}}{key (PK), value}
#' }
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
    body_text = pull("body"),
    roxygen_text = pull("roxygen"),
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
          query = entry$query %||% NA_character_,
          nodes_json = if (is.null(entry$nodes)) {
            NA_character_
          } else {
            jsonlite::toJSON(entry$nodes, auto_unbox = FALSE)
          },
          polarity = as.numeric(entry$polarity %||% 1.0),
          session_id = entry$session_id %||% NA_character_,
          created_at = entry$timestamp %||% NA_character_,
          stringsAsFactors = FALSE
        )
      },
      error = function(e) NULL
    )
  })

  rows <- do.call(rbind, Filter(Negate(is.null), rows))
  if (!is.null(rows) && nrow(rows) > 0L) {
    DBI::dbWriteTable(
      con,
      "task_traces",
      rows,
      append = TRUE,
      row.names = FALSE
    )
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
