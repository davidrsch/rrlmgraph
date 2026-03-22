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
  } # auto -> full (caller decides for supporting)

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
#'   \item **Header** -- project name, R version, approximate token count.
#'   \item **CORE FUNCTIONS** -- seed node rendered in full-source mode.
#'   \item **SUPPORTING FUNCTIONS** -- remaining user-function nodes in
#'     compressed mode.
#'   \item **FRAMEWORK / PACKAGE CONTEXT** -- package-type nodes in
#'     compressed mode.
#'   \item **RECENT TASK HISTORY** -- up to 3 entries from the
#'     \code{task_history} graph attribute; omitted when empty.
#'   \item **CONSTRAINTS** -- boilerplate footer reminding the LLM to
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

  # Approximate token count (chars / 3.5 heuristic — matches all other sites, rrlmgraph#95)
  n_tokens <- ceiling(nchar(body, type = "chars") / 3.5)

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

