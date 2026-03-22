# ---- Internal helpers ------------------------------------------------

#' Read task-trace rows from a SQLite \code{task_traces} table.
#'
#' Returns a data.frame with columns: \code{query}, \code{nodes_json},
#' \code{polarity}, \code{session_id}, \code{created_at}.
#' Returns \code{NULL} silently on any failure.
#'
#' @keywords internal
.read_traces_sqlite <- function(sqlite_path) {
  if (is.null(sqlite_path) || !nzchar(sqlite_path)) {
    return(NULL)
  }
  if (!file.exists(sqlite_path)) {
    return(NULL)
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    return(NULL)
  }
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    return(NULL)
  }

  tryCatch(
    {
      con <- DBI::dbConnect(
        RSQLite::SQLite(),
        sqlite_path,
        flags = RSQLite::SQLITE_RO
      )
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      tables <- DBI::dbListTables(con)
      if (!"task_traces" %in% tables) {
        return(NULL)
      }

      DBI::dbGetQuery(
        con,
        "SELECT query, nodes_json, polarity, session_id, created_at
       FROM task_traces
       ORDER BY created_at ASC"
      )
    },
    error = function(e) NULL
  )
}

#' @keywords internal
.trace_project_root <- function(graph) {
  pr <- tryCatch(
    igraph::graph_attr(graph, "project_root"),
    error = function(e) NULL
  )
  if (is.null(pr) || length(pr) == 0L || is.na(pr) || nchar(trimws(pr)) == 0L) {
    return(NULL)
  }
  pr
}

#' @keywords internal
.ensure_trace_file <- function(project_root) {
  if (is.null(project_root)) {
    return(NULL)
  }
  trace_dir <- file.path(project_root, ".rrlmgraph")
  dir.create(trace_dir, showWarnings = FALSE, recursive = TRUE)
  file.path(trace_dir, "task_trace.jsonl")
}

#' @keywords internal
.jsonl_entry <- function(query, nodes, polarity) {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::toJSON(
      list(
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        query = query,
        nodes = as.list(nodes),
        polarity = polarity,
        session_id = .tt_session_id()
      ),
      auto_unbox = TRUE
    )
  } else {
    # minimal fallback without jsonlite
    nodes_str <- paste0('["', paste(nodes, collapse = '","'), '"]')
    sprintf(
      '{"timestamp":"%s","query":"%s","nodes":%s,"polarity":%g,"session_id":"%s"}',
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      gsub('"', '\\"', query, fixed = TRUE),
      nodes_str,
      polarity,
      .tt_session_id()
    )
  }
}

#' @keywords internal
.tt_session_id <- function() {
  id <- Sys.getenv("RRLMGRAPH_SESSION_ID", unset = "")
  if (nchar(id) > 0L) {
    return(id)
  }
  format(proc.time()[["elapsed"]], digits = 10L)
}
