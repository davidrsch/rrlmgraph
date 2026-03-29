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
  # IDF(t) = log(1 + N / df(t)); N = total number of documents in corpus.
  # rrlmgraph#143 (issue #143): was using max(doc_count) as proxy for N,
  # which gives the LOWEST IDF (most common term) as the fallback. Unseen
  # terms should get HIGH IDF (maximum specificity). Use sum of doc_count as
  # a corpus-size proxy since total_docs is not separately tracked here.
  if (is.null(idf_weights) || length(idf_weights) == 0L) {
    n_docs <- sum(as.integer(vocab$doc_count), na.rm = TRUE)
    if (n_docs == 0L) {
      n_docs <- nrow(vocab)
    }
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
          polarity = as.numeric(entry$polarity %||% 0.0),
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
