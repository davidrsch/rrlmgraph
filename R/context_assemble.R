# R/context_assemble.R
# Node-context compression and LLM-ready string assembly.
# Covers rrlmgraph issues #11 and #12 (Sprint 2).

# ---- build_node_context (#11) ----------------------------------------

#' Build a text representation of a single graph node
#'
#' Returns a character string representing \code{node_name}.  Two modes
#' are supported:
#'
#' \describe{
#'   \item{\strong{full} (seed / primary nodes)}{Full function signature,
#'     Roxygen comment block (if present), and body text.  Used for the
#'     core nodes that the LLM must understand in detail.}
#'   \item{\strong{compressed} (supporting nodes / package nodes)}{A
#'     compact representation consisting of the signature, a one-line
#'     description, a \emph{Calls} list, and a \emph{Called by} list.
#'     Targets a compression ratio \eqn{\geq 3\times} vs.\ the full
#'     source (measured in approximate token units).}
#' }
#'
#' Package nodes (type \code{"package"}) always use compressed mode,
#' regardless of the \code{mode} argument.
#'
#' @param node_name Character(1).  The \code{node_id} of the vertex
#'   (e.g.\ \code{"utils::load_data"}).
#' @param graph An \code{rrlm_graph} / \code{igraph} object.
#' @param mode Character(1): \code{"auto"} (default), \code{"full"}, or
#'   \code{"compressed"}.  In \code{"auto"} the function returns full
#'   source; the caller should pass \code{"compressed"} explicitly for
#'   non-seed nodes.
#'
#' @return Character(1).  The text context for the node.  Empty string
#'   if the node is not found.
#'
#' @seealso [assemble_context_string()], [build_rrlm_graph()]
#' @export
#' @examples
#' \dontrun{
#' g   <- build_rrlm_graph("mypkg")
#' cat(build_node_context("utils::load_data", g, mode = "full"))
#' cat(build_node_context("utils::clean_data", g, mode = "compressed"))
#' }
build_node_context <- function(
  node_name,
  graph,
  mode = c("auto", "full", "compressed")
) {
  mode <- match.arg(mode)

  # Locate vertex --------------------------------------------------------
  v_idx <- match(node_name, igraph::V(graph)$name)
  if (is.na(v_idx)) {
    cli::cli_warn("build_node_context: node {.val {node_name}} not found.")
    return("")
  }

  node_type <- igraph::V(graph)$node_type[[v_idx]] %||% "function"

  # Package nodes are always compressed
  effective_mode <- if (node_type == "package") {
    "compressed"
  } else if (mode %in% c("full", "compressed")) {
    mode
  } else {
    "full"
  } # auto → full (caller decides for supporting)

  if (effective_mode == "full") {
    .ctx_full(v_idx, graph)
  } else {
    .ctx_compressed(v_idx, node_name, graph)
  }
}

# ---- assemble_context_string (#12) -----------------------------------

#' Assemble a structured, LLM-ready context string from ranked hits
#'
#' Takes an ordered list of node names (seed first) and returns a
#' formatted context string suitable for inclusion in a language-model
#' prompt.  The structure is:
#'
#' \enumerate{
#'   \item **Header** — project name, R version, approximate token count.
#'   \item **CORE FUNCTIONS** — seed node rendered in full-source mode.
#'   \item **SUPPORTING FUNCTIONS** — remaining user-function nodes in
#'     compressed mode.
#'   \item **FRAMEWORK / PACKAGE CONTEXT** — package-type nodes in
#'     compressed mode.
#'   \item **RECENT TASK HISTORY** — up to 3 entries from the
#'     \code{task_history} graph attribute; omitted when empty.
#'   \item **CONSTRAINTS** — boilerplate footer reminding the LLM to
#'     use only listed functions.
#' }
#'
#' @param hits Character vector.  Node names ordered by relevance, seed
#'   (most relevant) first.
#' @param graph An \code{rrlm_graph} / \code{igraph} object.
#' @param query Character(1).  The original user query string.  Included
#'   in the header for context.
#'
#' @return Character(1) valid UTF-8 string.  \code{nchar()} is stable
#'   across calls with identical inputs.
#'
#' @seealso [build_node_context()], [compute_relevance()]
#' @export
#' @examples
#' \dontrun{
#' g    <- build_rrlm_graph("mypkg")
#' hits <- c("utils::load_data", "utils::clean_data", "dplyr")
#' cat(assemble_context_string(hits, g, "load and clean training data"))
#' }
assemble_context_string <- function(hits, graph, query = "") {
  if (length(hits) == 0L) {
    return(.ctx_empty(query))
  }

  # Graph metadata -------------------------------------------------------
  pname <- igraph::graph_attr(graph, "project_name") %||% "unknown"
  rver <- igraph::graph_attr(graph, "r_version") %||% R.version$major
  history <- igraph::graph_attr(graph, "task_history") # NULL if absent

  # Classify nodes -------------------------------------------------------
  v_types <- .lookup_node_types(hits, graph)
  seed <- hits[[1L]]
  rest <- if (length(hits) > 1L) hits[2L:length(hits)] else character(0)

  user_rest <- rest[v_types[rest] %in% c("function", "testfile", NA_character_)]
  pkg_nodes <- rest[v_types[rest] == "package"]

  # Build sections -------------------------------------------------------
  core_text <- paste0(
    "### ",
    seed,
    "\n",
    build_node_context(seed, graph, mode = "full")
  )

  support_text <- if (length(user_rest) > 0L) {
    paste(
      vapply(
        user_rest,
        function(n) {
          paste0(
            "### ",
            n,
            "\n",
            build_node_context(n, graph, mode = "compressed")
          )
        },
        character(1)
      ),
      collapse = "\n\n"
    )
  } else {
    ""
  }

  pkg_text <- if (length(pkg_nodes) > 0L) {
    paste(
      vapply(
        pkg_nodes,
        function(n) {
          paste0(
            "### ",
            n,
            "\n",
            build_node_context(n, graph, mode = "compressed")
          )
        },
        character(1)
      ),
      collapse = "\n\n"
    )
  } else {
    ""
  }

  # Task history (max 3 entries) -----------------------------------------
  history_text <- .fmt_task_history(history)

  # Assemble body (without header, to count tokens first) ----------------
  sections <- list()
  sections <- c(
    sections,
    list(
      "## CORE FUNCTIONS",
      "---",
      core_text
    )
  )
  if (nzchar(trimws(support_text))) {
    sections <- c(
      sections,
      list(
        "\n## SUPPORTING FUNCTIONS",
        "---",
        support_text
      )
    )
  }
  if (nzchar(trimws(pkg_text))) {
    sections <- c(
      sections,
      list(
        "\n## FRAMEWORK / PACKAGE CONTEXT",
        "---",
        pkg_text
      )
    )
  }
  if (nzchar(trimws(history_text))) {
    sections <- c(
      sections,
      list(
        "\n## RECENT TASK HISTORY",
        "---",
        history_text
      )
    )
  }
  sections <- c(
    sections,
    list(
      "\n## CONSTRAINTS",
      "---",
      paste0(
        "Only use the functions and packages listed above. ",
        "Do not invent APIs, function names, or arguments not shown here. ",
        "If unsure, ask for clarification."
      )
    )
  )

  body <- paste(sections, collapse = "\n")

  # Approximate token count (chars / 4 heuristic) ------------------------
  n_tokens <- ceiling(nchar(body, type = "chars") / 4L)

  # Header ---------------------------------------------------------------
  header <- paste0(
    "# rrlm_graph Context\n",
    "# Project: ",
    pname,
    " | R ",
    rver,
    " | ~",
    n_tokens,
    " tokens\n",
    if (nzchar(query)) paste0("# Query: ", query, "\n") else ""
  )

  enc2utf8(paste0(header, "\n", body))
}

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
