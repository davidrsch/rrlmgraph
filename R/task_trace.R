# R/task_trace.R
# Recursive task-trace feedback loop for rrlmgraph.
# Covers rrlmgraph issue #18 (Sprint 3).

# ---- log_task_trace --------------------------------------------------

#' Log a task-trace entry to the JSONL feedback file
#'
#' Appends a single JSONL entry to
#' \file{.rrlmgraph/task_trace.jsonl} under the project root.  The
#' file is created automatically.  Each entry records the query, the
#' nodes that were surfaced, the feedback polarity, a timestamp, and the
#' current session identifier.
#'
#' @param query Character(1).  The user query.
#' @param nodes Character vector.  Node names that were part of the
#'   context returned.
#' @param graph An \code{rrlm_graph} / \code{igraph} object.  Used to
#'   resolve the project root from the \code{"project_root"} graph
#'   attribute.
#' @param polarity Numeric(1) in \eqn{[-1, 1]}.  Feedback signal:
#'   \eqn{> 0} means the context was helpful, \eqn{< 0} means it was
#'   not, \eqn{0} (default) means neutral.
#'
#' @return The path to \file{task_trace.jsonl}, invisibly.
#' @seealso [update_task_weights()], [update_task_polarity()]
#' @export
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' log_task_trace("How does load_data work?", c("pkg::load_data"), g)
#' }
log_task_trace <- function(query, nodes, graph, polarity = 0) {
  if (!is.character(query) || length(query) != 1L) {
    cli::cli_abort("{.arg query} must be a single character string.")
  }
  polarity <- as.numeric(polarity)
  if (is.na(polarity) || polarity < -1 || polarity > 1) {
    cli::cli_abort("{.arg polarity} must be a number in [-1, 1].")
  }

  project_root <- .trace_project_root(graph)
  trace_file <- .ensure_trace_file(project_root)

  entry <- .jsonl_entry(query, nodes, polarity)
  cat(entry, "\n", file = trace_file, append = TRUE, sep = "")

  invisible(trace_file)
}

# ---- update_task_weights ---------------------------------------------

#' Update task-trace weights on graph vertices
#'
#' Reads trace entries from \file{.rrlmgraph/task_trace.jsonl} and/or the
#' \code{task_traces} table in the project's \file{graph.sqlite} database,
#' applies exponential decay with a 30-day half-life
#' \eqn{(w = 2^{-\Delta d / 30})}, and accumulates per-node weights
#' as \eqn{\sum_i w_i \cdot (1 + \text{polarity}_i)}.  The result is
#' min-max-normalised to \eqn{[0, 1]} and stored in the
#' \code{task_trace_weight} vertex attribute.
#'
#' Reading from both sources ensures that traces written by the MCP server's
#' \code{add_task_trace} tool (which writes directly to SQLite) are
#' included alongside traces written by \code{log_task_trace()} (which
#' writes to JSONL).
#'
#' When no trace data exists anywhere, falls back to the exponential
#' moving-average (EMA) stub so that \code{query_context()} can still
#' boost \code{useful_nodes} in memory.
#'
#' @param graph An \code{rrlm_graph} / \code{igraph} object.
#' @param useful_nodes Character vector of node names that were helpful
#'   in the current traversal.  Also accepts \code{NULL} or
#'   \code{character(0)} to skip the EMA boost.
#' @param alpha Numeric(1).  EMA learning rate \eqn{(0, 1)}.  Default
#'   \code{0.3}.  Used only for the in-memory EMA boost; ignored when
#'   trace data is present.
#' @param decay Numeric(1).  Multiplicative decay applied to all other
#'   nodes in the EMA fallback path.  Default \code{0.99}.
#' @param trace_file Character(1) or \code{NULL}.  Explicit path to the
#'   JSONL file.  \code{NULL} (default) infers the path from the
#'   \code{"project_root"} graph attribute.
#' @param sqlite_path Character(1) or \code{NULL}.  Path to the SQLite
#'   database exported by \code{\link{export_to_sqlite}}.  \code{NULL}
#'   (default) infers the path as
#'   \file{.rrlmgraph/graph.sqlite} under the project root.  Set to
#'   \code{NA_character_} to skip SQLite lookup entirely.
#'
#' @return The graph with updated \code{task_trace_weight} vertex
#'   attributes.
#' @seealso [log_task_trace()], [update_task_polarity()]
#' @export
#' @examples
#' \dontrun{
#' g <- build_rrlm_graph("mypkg")
#' g <- update_task_weights(g, useful_nodes = c("pkg::load_data"))
#' }
update_task_weights <- function(
  graph,
  useful_nodes = NULL,
  alpha = 0.3,
  decay = 0.99,
  trace_file = NULL,
  sqlite_path = NULL
) {
  if (!inherits(graph, "igraph")) {
    cli::cli_abort("{.arg graph} must be an igraph / rrlm_graph object.")
  }
  if (igraph::vcount(graph) == 0L) {
    return(graph)
  }

  # ---- resolve trace file -----------------------------------------
  if (is.null(trace_file) || is.null(sqlite_path)) {
    project_root <- .trace_project_root(graph)
    if (!is.null(project_root)) {
      if (is.null(trace_file)) {
        trace_file <- file.path(project_root, ".rrlmgraph", "task_trace.jsonl")
      }
      if (is.null(sqlite_path)) {
        sqlite_path <- file.path(project_root, ".rrlmgraph", "graph.sqlite")
      }
    }
  }

  # ---- initialise weights -----------------------------------------
  cur <- igraph::V(graph)$task_trace_weight
  if (is.null(cur)) {
    igraph::V(graph)$task_trace_weight <- rep(0.5, igraph::vcount(graph))
    cur <- igraph::V(graph)$task_trace_weight
  }
  cur <- as.numeric(cur)
  cur[is.na(cur)] <- 0.5

  v_names <- igraph::V(graph)$name

  # ---- full trace path (JSONL + SQLite) ---------------------------
  # Collect trace rows from JSONL (R-side writes) and SQLite (MCP-side writes).
  all_rows <- NULL

  # --- JSONL source ---
  if (
    !is.null(trace_file) &&
      !is.na(trace_file) &&
      file.exists(trace_file) &&
      requireNamespace("jsonlite", quietly = TRUE)
  ) {
    lines <- readLines(trace_file, warn = FALSE)
    lines <- lines[nchar(trimws(lines)) > 0L]

    if (length(lines) > 0L) {
      jsonl_rows <- lapply(lines, function(line) {
        tryCatch(
          {
            entry <- jsonlite::fromJSON(line, simplifyVector = TRUE)
            list(
              created_at = as.character(entry$timestamp %||% NA_character_),
              nodes = as.character(unlist(entry$nodes)),
              polarity = as.numeric(entry$polarity %||% 0),
              session_id = as.character(entry$session_id %||% NA_character_)
            )
          },
          error = function(e) NULL
        )
      })
      all_rows <- c(all_rows, Filter(Negate(is.null), jsonl_rows))
    }
  }

  # --- SQLite source ---
  if (!is.null(sqlite_path) && !identical(sqlite_path, NA_character_)) {
    sq <- .read_traces_sqlite(sqlite_path)
    if (!is.null(sq) && nrow(sq) > 0L) {
      sq_rows <- lapply(seq_len(nrow(sq)), function(i) {
        nodes_raw <- tryCatch(
          if (requireNamespace("jsonlite", quietly = TRUE)) {
            as.character(unlist(jsonlite::fromJSON(
              sq$nodes_json[[i]],
              simplifyVector = TRUE
            )))
          } else {
            character(0)
          },
          error = function(e) character(0)
        )
        list(
          created_at = as.character(sq$created_at[[i]]),
          nodes = nodes_raw,
          polarity = as.numeric(sq$polarity[[i]]),
          session_id = as.character(sq$session_id[[i]])
        )
      })
      all_rows <- c(all_rows, sq_rows)
    }
  }

  # Deduplicate: remove rows with identical (session_id, created_at).
  if (length(all_rows) > 0L) {
    keys <- vapply(
      all_rows,
      function(r) {
        paste(r$session_id %||% "", r$created_at %||% "", sep = "\r")
      },
      character(1L)
    )
    all_rows <- all_rows[!duplicated(keys)]
  }

  if (length(all_rows) > 0L) {
    now_ts <- as.numeric(Sys.time())

    accum <- stats::setNames(
      rep(0.0, length(v_names)),
      v_names
    )

    for (row in all_rows) {
      entry_ts <- tryCatch(
        as.numeric(as.POSIXct(
          row$created_at[[1L]],
          format = "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        )),
        error = function(e) now_ts
      )
      delta_days <- max(0, (now_ts - entry_ts) / 86400)
      w_decay <- 2^(-delta_days / 30)

      pol <- max(-1, min(1, as.numeric(row$polarity %||% 0)))

      for (nd in row$nodes) {
        if (nd %in% v_names) {
          accum[[nd]] <- accum[[nd]] + w_decay * (1 + pol)
        }
      }
    }

    # min-max normalise to [0.1, 1.0]
    mx <- max(accum)
    if (mx > 0) {
      cur <- 0.1 + 0.9 * (accum / mx)
    }

    igraph::V(graph)$task_trace_weight <- cur
    return(graph)
  }

  # ---- EMA fallback -----------------------------------------------
  cur <- cur * decay

  idx <- match(useful_nodes, v_names)
  idx <- idx[!is.na(idx)]
  if (length(idx) > 0L) {
    cur[idx] <- cur[idx] * (1 - alpha) + 1.0 * alpha
  }

  cur <- pmin(1, pmax(0, cur))
  igraph::V(graph)$task_trace_weight <- cur
  graph
}

# ---- update_task_polarity --------------------------------------------

#' Update the polarity of recent task-trace entries
#'
#' Finds trace entries whose node sets overlap with \code{context$nodes}
#' and rewrites their \code{polarity} field.  Only the most recent
#' \code{n_recent} matching entries are updated (default: 3).
#'
#' @param graph An \code{rrlm_graph} / \code{igraph} object.  Used to
#'   resolve the project root.
#' @param context An \code{rrlm_context} object (output of
#'   \code{query_context()}).  The \code{$nodes} field is used to match
#'   trace entries.
#' @param polarity Numeric(1) in \eqn{[-1, 1]}.  New polarity value to
#'   apply.
#' @param n_recent Integer(1).  Maximum number of recent matching
#'   entries to update.  Default \code{3L}.
#'
#' @return The graph, invisibly (no in-memory state change; the JSONL
#'   file is rewritten).
#' @seealso [log_task_trace()], [update_task_weights()]
#' @export
#' @examples
#' \dontrun{
#' g   <- build_rrlm_graph("mypkg")
#' ctx <- query_context(g, "parse CSV files")
#' # User says context was helpful:
#' update_task_polarity(g, ctx, polarity = 0.8)
#' }
update_task_polarity <- function(graph, context, polarity, n_recent = 3L) {
  if (!inherits(context, "rrlm_context")) {
    cli::cli_abort("{.arg context} must be an {.cls rrlm_context} object.")
  }
  polarity <- as.numeric(polarity)
  if (is.na(polarity) || polarity < -1 || polarity > 1) {
    cli::cli_abort("{.arg polarity} must be a number in [-1, 1].")
  }

  project_root <- .trace_project_root(graph)
  if (is.null(project_root)) {
    cli::cli_warn(
      "Cannot update polarity: {.arg graph} has no {.val project_root} attribute."
    )
    return(invisible(graph))
  }

  trace_file <- file.path(project_root, ".rrlmgraph", "task_trace.jsonl")
  if (!file.exists(trace_file)) {
    return(invisible(graph))
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cli::cli_warn(
      "Install {.pkg jsonlite} to use {.fn update_task_polarity}."
    )
    return(invisible(graph))
  }

  lines <- readLines(trace_file, warn = FALSE)
  ctx_nodes <- context$nodes

  # Find indices of matching entries (working from the end = most recent)
  match_idx <- integer(0)
  for (i in rev(seq_along(lines))) {
    if (nchar(trimws(lines[[i]])) == 0L) {
      next
    }
    entry <- tryCatch(
      jsonlite::fromJSON(lines[[i]], simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(entry)) {
      next
    }

    entry_nodes <- as.character(unlist(entry$nodes))
    if (length(intersect(entry_nodes, ctx_nodes)) > 0L) {
      match_idx <- c(match_idx, i)
      if (length(match_idx) >= n_recent) break
    }
  }

  if (length(match_idx) == 0L) {
    return(invisible(graph))
  }

  # Rewrite matched lines with new polarity
  for (i in match_idx) {
    entry <- tryCatch(
      jsonlite::fromJSON(lines[[i]], simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(entry)) {
      next
    }
    entry$polarity <- polarity
    lines[[i]] <- jsonlite::toJSON(entry, auto_unbox = TRUE)
  }

  writeLines(lines, trace_file)
  invisible(graph)
}

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
