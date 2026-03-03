#' Build CALLS edges between function nodes
#'
#' Cross-references each function node's `calls_list` against the set of
#' known user-defined node identifiers.  Each matched call produces one
#' directed edge `(from_node_id -> to_node_id)`.
#'
#' The matching strategy is:
#' 1. Exact match on bare function name (`to_name`).
#' 2. Qualified match: if `calls_list` contains `pkg::fn` or `pkg:::fn`,
#'    the bare `fn` part is matched against known node names.
#'
#' Only edges between nodes present in `func_nodes` are returned (intra-project
#' edges).  Calls to external packages are intentionally excluded here; use
#' [build_import_edges()] for package-level dependencies.
#'
#' @param func_nodes A list of node records as returned by
#'   [extract_function_nodes()].
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{`from`}{Character: `node_id` of the calling function.}
#'     \item{`to`}{Character: `node_id` of the called function.}
#'     \item{`weight`}{Numeric: edge weight (default `1`).}
#'   }
#'   Zero rows if no intra-project calls are found.
#'
#' @seealso [build_import_edges()], [build_test_edges()]
#' @export
#' @examples
#' \dontrun{
#' proj  <- detect_rproject("/path/to/mypkg")
#' nodes <- extract_function_nodes(proj$r_files)
#' edges <- build_call_edges(nodes)
#' }
build_call_edges <- function(func_nodes) {
  if (length(func_nodes) == 0L) {
    return(.empty_edge_df())
  }

  # Build lookup: bare_name -> vector of node_ids (many nodes can share a name
  # across files; retain all so we keep the best match later)
  node_ids <- vapply(func_nodes, `[[`, character(1), "node_id")
  node_names <- vapply(func_nodes, `[[`, character(1), "name")
  name_to_ids <- split(node_ids, node_names)

  # Use list accumulation to avoid O(n²) c() copying inside loops.
  rows <- list()
  k <- 0L

  for (node in func_nodes) {
    calls <- unique(node$calls_list)
    if (length(calls) == 0L) {
      next
    }

    for (sym in calls) {
      # Try bare match first
      bare <- sub("^.*:::?", "", sym) # strip pkg:: or pkg:::
      candidates <- name_to_ids[[bare]]
      if (is.null(candidates)) {
        next
      }

      # Self-calls are valid edges (recursion)
      for (target_id in candidates) {
        k <- k + 1L
        rows[[k]] <- list(from = node$node_id, to = target_id)
      }
    }
  }

  if (k == 0L) {
    return(.empty_edge_df())
  }

  unique(data.frame(
    from = vapply(rows, `[[`, character(1L), "from"),
    to = vapply(rows, `[[`, character(1L), "to"),
    weight = 1,
    stringsAsFactors = FALSE
  ))
}

#' Build IMPORTS edges from R files to package dependencies
#'
#' Discovers package dependencies at two levels:
#'
#' 1. **Explicit** -- `library(pkg)` and `require(pkg)` calls in source files.
#' 2. **Qualified** -- `pkg::fn` and `pkg:::fn` patterns (the package name is
#'    extracted as an import).
#' 3. **DESCRIPTION** -- if a `DESCRIPTION` file is found in the project root
#'    (or any ancestor up to two levels up), its `Imports:` and `Depends:`
#'    fields are parsed.
#'
#' Each discovered dependency produces an edge from the *file stem* (or the
#' project root stem) to the package name.  Base-R packages (`base`, `methods`,
#' `utils`, `stats`, `datasets`, `graphics`, `grDevices`) are included when
#' explicitly referenced but are labelled with `source = "qualified"` or
#' `source = "description"` for downstream filtering.
#'
#' @param r_files  Character vector of absolute paths to `.R` source files.
#' @param root     Optional character(1). Project root directory.  When
#'   provided, a `DESCRIPTION` file is searched there.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{`from`}{File stem (or project identifier) where the import appears.}
#'     \item{`to`}{Package name being imported.}
#'     \item{`weight`}{Numeric: edge weight (default `1`).}
#'     \item{`source`}{Character: `"library"`, `"qualified"`, or `"description"`.}
#'   }
#'
#' @seealso [build_call_edges()], [build_test_edges()]
#' @export
#' @examples
#' \dontrun{
#' proj  <- detect_rproject("/path/to/mypkg")
#' edges <- build_import_edges(proj$r_files, root = proj$root)
#' }
build_import_edges <- function(r_files, root = NULL) {
  rows <- list()

  for (fpath in r_files) {
    src <- tryCatch(readLines(fpath, warn = FALSE), error = function(e) {
      character(0)
    })
    if (length(src) == 0L) {
      next
    }

    fstem <- as.character(fs::path_ext_remove(fs::path_file(fpath)))

    # ---- 1. library() / require() ----------------------------------------
    lib_matches <- regmatches(
      src,
      gregexpr(
        "(?:library|require)\\s*\\(\\s*['\"]?([A-Za-z][A-Za-z0-9._]*)['\"]?\\s*\\)",
        src,
        perl = TRUE
      )
    )
    for (m_vec in lib_matches) {
      for (m in m_vec) {
        pkg <- sub(
          ".*(?:library|require)\\s*\\(\\s*['\"]?([A-Za-z][A-Za-z0-9._]*)['\"]?.*",
          "\\1",
          m,
          perl = TRUE
        )
        if (nchar(pkg) > 0L) {
          rows <- c(
            rows,
            list(data.frame(
              from = fstem,
              to = pkg,
              weight = 1,
              source = "library",
              stringsAsFactors = FALSE
            ))
          )
        }
      }
    }

    # ---- 2. pkg::fn qualified calls --------------------------------------
    qual_matches <- regmatches(
      src,
      gregexpr(
        "([A-Za-z][A-Za-z0-9._]*):::?[A-Za-z._][A-Za-z0-9._]*",
        src,
        perl = TRUE
      )
    )
    for (m_vec in qual_matches) {
      for (m in m_vec) {
        pkg <- sub(":::?.*$", "", m)
        if (nchar(pkg) > 0L) {
          rows <- c(
            rows,
            list(data.frame(
              from = fstem,
              to = pkg,
              weight = 1,
              source = "qualified",
              stringsAsFactors = FALSE
            ))
          )
        }
      }
    }
  }

  # ---- 3. DESCRIPTION Imports / Depends --------------------------------
  if (!is.null(root)) {
    desc_path <- .find_description(root)
    if (!is.null(desc_path)) {
      desc_rows <- .parse_description_deps(desc_path)
      rows <- c(rows, desc_rows)
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      from = character(0),
      to = character(0),
      weight = numeric(0),
      source = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # De-duplicate, keeping first source priority: library > description > qualified
  result <- unique(do.call(rbind, rows))
  result[order(result$from, result$to), ]
}

#' Build TEST edges from test files to user-defined functions
#'
#' Parses each test file for references to user-defined function names, using
#' the same AST-walking approach used in `find_calls_in_body()`.  A `TEST`
#' edge `(test_file_stem -> function_node_id)` is emitted when a test file
#' calls a function that exists as a node in `func_nodes`.
#'
#' Test-helper symbols (`expect_*`, `test_that`, `describe`, `it`, `setup`,
#' `teardown`, `skip*`, `withr::*`) are excluded from matching to avoid false
#' edges to non-existent nodes.
#'
#' @param func_nodes A list of function node records from
#'   [extract_function_nodes()].
#' @param test_files Character vector of absolute paths to test `.R` files.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{`from`}{Test file stem, character.}
#'     \item{`to`}{`node_id` of the tested function, character.}
#'     \item{`weight`}{Numeric: edge weight (default `1`).}
#'   }
#'
#' @seealso [build_call_edges()], [build_import_edges()]
#' @export
#' @examples
#' \dontrun{
#' proj  <- detect_rproject("/path/to/mypkg")
#' nodes <- extract_function_nodes(proj$r_files)
#' edges <- build_test_edges(nodes, proj$test_files)
#' }
build_test_edges <- function(func_nodes, test_files) {
  if (length(func_nodes) == 0L || length(test_files) == 0L) {
    return(.empty_edge_df())
  }

  # Build lookup: bare_name -> node_id (prefer first match when duplicates)
  node_ids <- vapply(func_nodes, `[[`, character(1), "node_id")
  node_names <- vapply(func_nodes, `[[`, character(1), "name")
  name_to_id <- setNames(node_ids, node_names)

  # Symbols to exclude: testthat + withr helpers
  test_helpers <- c(
    "test_that",
    "describe",
    "it",
    "expect_equal",
    "expect_identical",
    "expect_true",
    "expect_false",
    "expect_null",
    "expect_na",
    "expect_error",
    "expect_warning",
    "expect_message",
    "expect_no_error",
    "expect_no_warning",
    "expect_no_message",
    "expect_length",
    "expect_named",
    "expect_type",
    "expect_s3_class",
    "expect_s4_class",
    "expect_vector",
    "expect_output",
    "expect_gt",
    "expect_gte",
    "expect_lt",
    "expect_lte",
    "expect_setequal",
    "expect_contains",
    "expect_in",
    "setup",
    "teardown",
    "skip",
    "skip_if",
    "skip_if_not",
    "skip_if_not_installed",
    "skip_on_cran",
    "skip_on_ci",
    "local_tempdir",
    "local_tempfile",
    "source",
    "library",
    "require"
  )

  # Use list accumulation to avoid O(n²) c() copying.
  rows <- list()
  k <- 0L

  for (tpath in test_files) {
    parsed <- tryCatch(
      parse(tpath, keep.source = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      next
    }

    # Use find_calls_in_body on each expression
    calls <- character(0)
    for (expr in as.list(parsed)) {
      new_calls <- tryCatch(
        find_calls_in_body(expr),
        error = function(e) character(0)
      )
      calls <- c(calls, new_calls)
    }
    calls <- setdiff(unique(calls), test_helpers)

    fstem <- as.character(fs::path_ext_remove(fs::path_file(tpath)))

    for (sym in calls) {
      bare <- sub("^.*:::?", "", sym)
      if (!nzchar(bare) || !bare %in% names(name_to_id)) {
        next
      }
      nid <- name_to_id[[bare]]
      if (!is.null(nid)) {
        k <- k + 1L
        rows[[k]] <- list(from = fstem, to = nid)
      }
    }
  }

  if (k == 0L) {
    return(.empty_edge_df())
  }

  unique(data.frame(
    from = vapply(rows, `[[`, character(1L), "from"),
    to = vapply(rows, `[[`, character(1L), "to"),
    weight = 1,
    stringsAsFactors = FALSE
  ))
}

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

# ---- internal helpers -----------------------------------------------

#' @keywords internal
.empty_edge_df <- function() {
  data.frame(
    from = character(0),
    to = character(0),
    weight = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.empty_dispatch_edge_df <- function() {
  data.frame(
    from = character(0),
    to = character(0),
    weight = numeric(0),
    edge_type = character(0),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.git_available <- function(project_root) {
  git_dir <- file.path(as.character(project_root), ".git")
  if (!file.exists(git_dir)) {
    return(FALSE)
  }
  tryCatch(
    identical(
      system2(
        "git",
        c(
          "-C",
          shQuote(as.character(project_root)),
          "rev-parse",
          "--is-inside-work-tree"
        ),
        stdout = FALSE,
        stderr = FALSE
      ),
      0L
    ),
    error = function(e) FALSE
  )
}

#' @keywords internal
.parse_git_log_files <- function(lines) {
  result <- list()
  current_hash <- NULL
  current_files <- character(0)
  past_blank <- FALSE

  for (line in lines) {
    trimmed <- trimws(line)
    if (grepl("^[0-9a-f]{40}$", trimmed, perl = TRUE)) {
      if (!is.null(current_hash) && length(current_files) > 0L) {
        result <- c(result, list(current_files))
      }
      current_hash <- trimmed
      current_files <- character(0)
      past_blank <- FALSE
    } else if (nchar(trimmed) == 0L) {
      past_blank <- TRUE
    } else if (past_blank && nchar(trimmed) > 0L) {
      current_files <- c(current_files, trimmed)
    }
  }
  if (!is.null(current_hash) && length(current_files) > 0L) {
    result <- c(result, list(current_files))
  }
  result
}

#' Extract pairs of regex capture groups from all matches in \code{text}.
#' \code{pattern} must have exactly two capturing groups.
#' Returns a list of \code{character(2)} vectors, one per match.
#' @keywords internal
.regex_all_captures2 <- function(text, pattern) {
  matches <- gregexpr(pattern, text, perl = TRUE)
  result <- list()
  for (i in seq_along(text)) {
    m <- matches[[i]]
    if (length(m) == 0L || m[[1L]] == -1L) {
      next
    }
    starts <- attr(m, "capture.start")
    lengths <- attr(m, "capture.length")
    if (is.null(starts) || ncol(starts) < 2L) {
      next
    }
    for (j in seq_len(nrow(starts))) {
      g1 <- substr(
        text[[i]],
        starts[j, 1L],
        starts[j, 1L] + lengths[j, 1L] - 1L
      )
      g2 <- substr(
        text[[i]],
        starts[j, 2L],
        starts[j, 2L] + lengths[j, 2L] - 1L
      )
      if (nzchar(g1) && nzchar(g2)) {
        result <- c(result, list(c(g1, g2)))
      }
    }
  }
  result
}

#' @keywords internal
.find_description <- function(root) {
  # Look in root and up to 2 levels up
  for (i in 0:2) {
    candidate <- fs::path(root, strrep("../", i), "DESCRIPTION")
    norm_path <- tryCatch(fs::path_abs(candidate), error = function(e) NULL)
    if (!is.null(norm_path) && fs::file_exists(norm_path)) {
      return(as.character(norm_path))
    }
  }
  NULL
}

#' @keywords internal
.parse_description_deps <- function(desc_path) {
  tryCatch(
    {
      desc <- read.dcf(desc_path)
      fields <- c("Imports", "Depends")
      rows <- list()

      for (field in fields) {
        if (!field %in% colnames(desc)) {
          next
        }
        raw <- desc[1L, field]
        if (is.na(raw) || nchar(trimws(raw)) == 0L) {
          next
        }

        # Split on comma, extract bare package name (strip version constraint)
        pkgs <- trimws(strsplit(raw, ",")[[1L]])
        pkgs <- sub("\\s*\\(.*\\)$", "", pkgs) # remove (>= x.y.z)
        pkgs <- pkgs[nchar(pkgs) > 0L & pkgs != "R"]

        project_stem <- as.character(
          fs::path_ext_remove(fs::path_file(desc_path))
        )
        for (pkg in pkgs) {
          rows <- c(
            rows,
            list(data.frame(
              from = project_stem,
              to = pkg,
              weight = 1,
              source = "description",
              stringsAsFactors = FALSE
            ))
          )
        }
      }
      rows
    },
    error = function(e) list()
  )
}

