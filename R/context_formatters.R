# ---- internal --------------------------------------------------------

#' @keywords internal
.ctx_full <- function(v_idx, graph) {
  sig <- igraph::V(graph)$signature[[v_idx]] %||% ""
  rox <- igraph::V(graph)$roxygen_text[[v_idx]] %||% ""
  body <- igraph::V(graph)$body_text[[v_idx]] %||% ""
  name <- igraph::V(graph)$name[[v_idx]]

  parts <- character(0)
  if (nzchar(trimws(rox))) {
    parts <- c(parts, trimws(rox))
  }
  if (nzchar(trimws(sig))) {
    # Format as: fn_name <- function(args) { ... body ... }
    fn_decl <- paste0(sig, " {")
    if (nzchar(trimws(body))) {
      fn_decl <- paste0(fn_decl, "\n", body, "\n}")
    } else {
      fn_decl <- paste0(fn_decl, "}")
    }
    parts <- c(parts, fn_decl)
  } else if (nzchar(trimws(body))) {
    parts <- c(parts, body)
  }
  paste(parts, collapse = "\n")
}

#' @keywords internal
.ctx_compressed <- function(v_idx, node_name, graph) {
  sig <- igraph::V(graph)$signature[[v_idx]] %||% node_name
  rox <- igraph::V(graph)$roxygen_text[[v_idx]] %||% ""
  node_type <- igraph::V(graph)$node_type[[v_idx]] %||% "function"

  # One-line description: first non-empty Roxygen sentence, else signature
  desc <- .extract_one_liner(rox)

  # Calls (outgoing CALLS edges from this node)
  out_nbrs <- tryCatch(
    {
      igraph::neighbors(graph, v_idx, mode = "out")
    },
    error = function(e) igraph::V(graph)[integer(0)]
  )
  call_names <- igraph::V(graph)$name[out_nbrs]
  # Filter to just CALLS edges
  out_edges <- tryCatch(
    igraph::incident(graph, v_idx, mode = "out"),
    error = function(e) integer(0)
  )
  if (length(out_edges) > 0L) {
    et <- igraph::E(graph)$edge_type[as.integer(out_edges)]
    call_ends <- igraph::ends(graph, out_edges[et == "CALLS"], names = TRUE)
    call_names <- if (length(out_edges[et == "CALLS"]) > 0L) {
      unique(call_ends[, 2L])
    } else {
      character(0)
    }
  }

  # Called by (incoming CALLS edges to this node)
  in_edges <- tryCatch(
    igraph::incident(graph, v_idx, mode = "in"),
    error = function(e) integer(0)
  )
  calledby_names <- character(0)
  if (length(in_edges) > 0L) {
    et <- igraph::E(graph)$edge_type[as.integer(in_edges)]
    cb_ends <- igraph::ends(graph, in_edges[et == "CALLS"], names = TRUE)
    calledby_names <- if (length(in_edges[et == "CALLS"]) > 0L) {
      unique(cb_ends[, 1L])
    } else {
      character(0)
    }
  }

  lines <- c(
    sig,
    if (nzchar(desc)) desc,
    if (length(call_names) > 0L) {
      paste0("Calls: ", paste(call_names, collapse = ", "))
    },
    if (length(calledby_names) > 0L) {
      paste0("Called by: ", paste(calledby_names, collapse = ", "))
    }
  )
  paste(lines, collapse = "\n")
}

#' @keywords internal
.extract_one_liner <- function(roxygen_text) {
  if (!nzchar(trimws(roxygen_text))) {
    return("")
  }
  lines <- strsplit(roxygen_text, "\n", fixed = TRUE)[[1L]]
  # Strip #' prefix
  lines <- trimws(sub("^#'\\s?", "", lines))
  # Find first non-empty, non-tag line
  non_tag <- lines[nzchar(lines) & !grepl("^@", lines)]
  if (length(non_tag) > 0L) non_tag[[1L]] else ""
}

#' @keywords internal
.lookup_node_types <- function(nodes, graph) {
  v_names <- igraph::V(graph)$name
  v_types <- igraph::V(graph)$node_type
  if (is.null(v_types)) {
    v_types <- rep(NA_character_, length(v_names))
  }
  idx <- match(nodes, v_names)
  types <- ifelse(is.na(idx), NA_character_, v_types[idx])
  stats::setNames(types, nodes)
}

#' @keywords internal
.fmt_task_history <- function(history) {
  if (is.null(history) || length(history) == 0L) {
    return("")
  }
  # history is expected to be a list of named character vectors or strings
  n <- length(history)
  entries <- history[max(1L, n - 2L):n]
  lines <- vapply(
    seq_along(entries),
    function(i) {
      e <- entries[[i]]
      if (is.character(e)) {
        paste0(i, ". ", paste(e, collapse = " "))
      } else {
        paste0(i, ". (unknown)")
      }
    },
    character(1)
  )
  paste(lines, collapse = "\n")
}

#' @keywords internal
.ctx_empty <- function(query) {
  enc2utf8(paste0(
    "# rrlm_graph Context\n",
    if (nzchar(query)) paste0("# Query: ", query, "\n") else "",
    "\n## CONSTRAINTS\n---\n",
    "No relevant functions found. Provide more context or broaden the query."
  ))
}
