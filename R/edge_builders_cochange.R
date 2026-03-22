# ---- build_co_change_edges ------------------------------------------

#' Build CO_CHANGES edges from git commit history
#'
#' Scans the git log and identifies R source files that were modified
#' together in the same commit.  Function nodes in those files receive
#' bidirectional \code{CO_CHANGES} edges with weight proportional to
#' how often the files change together (capped at \code{1.0}).
#'
#' Requires that \code{project_root} is inside a git repository.  Returns
#' zero rows silently when git is unavailable or the project has no commit
#' history with co-changed R files.
#'
#' @param func_nodes A list of node records from
#'   [extract_function_nodes()].
#' @param project_root Character(1).  Root of the git repository.
#'   Defaults to \code{"."}.
#' @param min_cochanges Integer(1).  Minimum number of co-change commits
#'   required to create an edge.  Default \code{2L}.
#'
#' @return A \code{data.frame} with columns \code{from}, \code{to},
#'   \code{weight}.  Zero rows when no qualifying pairs are found.
#'
#' @seealso [build_call_edges()], [build_rrlm_graph()]
#' @export
#' @examples
#' \dontrun{
#' proj  <- detect_rproject("/path/to/mypkg")
#' nodes <- extract_function_nodes(proj$r_files)
#' edges <- build_co_change_edges(nodes, proj$root)
#' }
build_co_change_edges <- function(
  func_nodes,
  project_root = ".",
  min_cochanges = 2L
) {
  if (length(func_nodes) == 0L) {
    return(.empty_edge_df())
  }
  project_root <- as.character(fs::path_abs(project_root))
  if (!.git_available(project_root)) {
    warning(
      "[rrlmgraph] CO_CHANGE edges skipped: git is unavailable or '",
      project_root,
      "' is not inside a git repository. ",
      "Graphs built without git will have no CO_CHANGES signal. ",
      "Relevance scores from this graph will differ from those built ",
      "in an environment where git history is accessible.",
      call. = FALSE
    )
    return(.empty_edge_df())
  }

  git_lines <- tryCatch(
    system2(
      "git",
      c(
        "-C",
        shQuote(project_root),
        "log",
        "--name-only",
        "--format=%H",
        "--diff-filter=ACMR",
        "--",
        "*.R"
      ),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) character(0)
  )
  if (length(git_lines) == 0L) {
    return(.empty_edge_df())
  }

  commit_files <- .parse_git_log_files(git_lines)
  if (length(commit_files) == 0L) {
    return(.empty_edge_df())
  }

  node_ids <- vapply(func_nodes, `[[`, character(1), "node_id")
  node_files <- vapply(
    func_nodes,
    function(n) n$file %||% NA_character_,
    character(1)
  )
  node_files_norm <- normalizePath(node_files, winslash = "/", mustWork = FALSE)

  total_commits <- length(commit_files)
  co_counts <- list()

  for (files in commit_files) {
    files_norm <- normalizePath(
      file.path(project_root, files),
      winslash = "/",
      mustWork = FALSE
    )
    relevant <- unique(files_norm[
      !is.na(node_files_norm) & files_norm %in% node_files_norm
    ])
    if (length(relevant) < 2L) {
      next
    }
    pairs <- utils::combn(sort(relevant), 2L, simplify = FALSE)
    for (pair in pairs) {
      key <- paste(pair, collapse = "\t")
      co_counts[[key]] <- (co_counts[[key]] %||% 0L) + 1L
    }
  }

  if (length(co_counts) == 0L) {
    return(.empty_edge_df())
  }

  rows <- list()
  k <- 0L
  min_n <- as.integer(min_cochanges)

  for (key in names(co_counts)) {
    n_together <- co_counts[[key]]
    if (n_together < min_n) {
      next
    }
    pair_files <- strsplit(key, "\t", fixed = TRUE)[[1L]]
    f1_nodes <- node_ids[
      !is.na(node_files_norm) & node_files_norm == pair_files[[1L]]
    ]
    f2_nodes <- node_ids[
      !is.na(node_files_norm) & node_files_norm == pair_files[[2L]]
    ]
    if (length(f1_nodes) == 0L || length(f2_nodes) == 0L) {
      next
    }
    w <- min(1.0, n_together / max(1L, total_commits))
    for (n1 in f1_nodes) {
      for (n2 in f2_nodes) {
        k <- k + 1L
        rows[[k]] <- list(from = n1, to = n2, weight = w)
        k <- k + 1L
        rows[[k]] <- list(from = n2, to = n1, weight = w)
      }
    }
  }

  if (k == 0L) {
    return(.empty_edge_df())
  }

  unique(data.frame(
    from = vapply(rows, `[[`, character(1), "from"),
    to = vapply(rows, `[[`, character(1), "to"),
    weight = vapply(rows, `[[`, numeric(1), "weight"),
    stringsAsFactors = FALSE
  ))
}

# ---- build_dispatch_edges -------------------------------------------

#' Build DISPATCHES_ON and EXTENDS edges for OOP patterns
#'
#' Scans R source files for S4 generics/methods, R5 reference classes,
#' and R6 classes.  Emits:
#' \describe{
#'   \item{\code{EXTENDS}}{From child class node to parent, detected via
#'     \code{setClass(contains=)}, \code{R6Class(inherit=)}, and
#'     \code{setRefClass(contains=)}.}
#'   \item{\code{DISPATCHES_ON}}{From a concrete S4 method node to the
#'     corresponding generic node when both appear in the parsed node set.}
#' }
#'
#' @param func_nodes A list of node records from [extract_function_nodes()].
#' @param r_files Character vector of absolute paths to \file{.R} source
#'   files.
#'
#' @return A \code{data.frame} with columns \code{from}, \code{to},
#'   \code{weight}, and \code{edge_type}.  Zero rows when no OOP patterns
#'   are detected.
#'
#' @seealso [build_call_edges()], [build_rrlm_graph()]
#' @export
#' @examples
#' \dontrun{
#' proj  <- detect_rproject("/path/to/mypkg")
#' nodes <- extract_function_nodes(proj$r_files)
#' edges <- build_dispatch_edges(nodes, proj$r_files)
#' }
build_dispatch_edges <- function(func_nodes, r_files) {
  if (length(func_nodes) == 0L || length(r_files) == 0L) {
    return(.empty_dispatch_edge_df())
  }

  node_ids <- vapply(func_nodes, `[[`, character(1), "node_id")
  node_names <- vapply(func_nodes, `[[`, character(1), "name")
  name_to_id <- stats::setNames(node_ids, node_names)

  rows <- list()
  k <- 0L

  .add_oop_edge <- function(child, parent, type) {
    cid <- name_to_id[[child]]
    pid <- name_to_id[[parent]]
    if (!is.null(cid) && !is.null(pid) && !identical(cid, pid)) {
      k <<- k + 1L
      rows[[k]] <<- list(from = cid, to = pid, edge_type = type)
    }
  }

  for (fpath in r_files) {
    src <- tryCatch(
      readLines(fpath, warn = FALSE),
      error = function(e) character(0)
    )
    if (length(src) == 0L) {
      next
    }
    full <- paste(src, collapse = "\n")

    # setClass("Child", ..., contains = "Parent") → EXTENDS
    for (caps in .regex_all_captures2(
      full,
      'setClass\\("([A-Za-z._][A-Za-z0-9._]*)"[\\s\\S]{0,500}?\\bcontains\\s*=\\s*"([A-Za-z._][A-Za-z0-9._]*)"'
    )) {
      .add_oop_edge(caps[[1L]], caps[[2L]], "EXTENDS")
    }

    # R6Class("Child", ..., inherit = Parent) → EXTENDS
    for (caps in .regex_all_captures2(
      full,
      'R6Class\\("([A-Za-z._][A-Za-z0-9._]*)"[\\s\\S]{0,500}?\\binherit\\s*=\\s*([A-Za-z._][A-Za-z0-9._]*)'
    )) {
      .add_oop_edge(caps[[1L]], caps[[2L]], "EXTENDS")
    }

    # setRefClass("Child", ..., contains = "Parent") → EXTENDS
    for (caps in .regex_all_captures2(
      full,
      'setRefClass\\("([A-Za-z._][A-Za-z0-9._]*)"[\\s\\S]{0,500}?\\bcontains\\s*=\\s*"([A-Za-z._][A-Za-z0-9._]*)"'
    )) {
      .add_oop_edge(caps[[1L]], caps[[2L]], "EXTENDS")
    }

    # DISPATCHES_ON: setMethod("name") when a same-named setGeneric exists.
    # gregexpr() on a single string returns a 1-element list; work with [[1L]].
    .extract_first_capture <- function(pattern) {
      m <- gregexpr(pattern, full, perl = TRUE)[[1L]]
      if (m[[1L]] == -1L) {
        return(character(0))
      }
      st <- attr(m, "capture.start")[, 1L, drop = TRUE]
      ln <- attr(m, "capture.length")[, 1L, drop = TRUE]
      unique(substr(rep(full, length(st)), st, st + ln - 1L))
    }

    generic_names_here <- .extract_first_capture(
      'setGeneric\\("([A-Za-z._][A-Za-z0-9._]*)"'
    )
    generic_names_here <- generic_names_here[nzchar(generic_names_here)]

    meth_names_here <- .extract_first_capture(
      'setMethod\\("([A-Za-z._][A-Za-z0-9._]*)"'
    )
    meth_names_here <- meth_names_here[nzchar(meth_names_here)]

    dispatch_names <- intersect(
      meth_names_here,
      c(generic_names_here, node_names)
    )
    for (mname in dispatch_names) {
      same_name <- node_ids[node_names == mname]
      if (length(same_name) >= 2L) {
        for (i in seq(2L, length(same_name))) {
          k <- k + 1L
          rows[[k]] <- list(
            from = same_name[[i]],
            to = same_name[[1L]],
            edge_type = "DISPATCHES_ON"
          )
        }
      }
    }
  }

  if (k == 0L) {
    return(.empty_dispatch_edge_df())
  }

  unique(data.frame(
    from = vapply(rows, `[[`, character(1), "from"),
    to = vapply(rows, `[[`, character(1), "to"),
    weight = 1.0,
    edge_type = vapply(rows, `[[`, character(1), "edge_type"),
    stringsAsFactors = FALSE
  ))
}

