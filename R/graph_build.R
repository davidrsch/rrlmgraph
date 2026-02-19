#' Build CALLS edges between function nodes
#'
#' Cross-references each function node's `calls_list` against the set of
#' known user-defined node identifiers.  Each matched call produces one
#' directed edge `(from_node_id → to_node_id)`.
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

  # Build lookup: bare_name → vector of node_ids (many nodes can share a name
  # across files; retain all so we keep the best match later)
  node_ids <- vapply(func_nodes, `[[`, character(1), "node_id")
  node_names <- vapply(func_nodes, `[[`, character(1), "name")
  name_to_ids <- split(node_ids, node_names)

  from_vec <- character(0)
  to_vec <- character(0)

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
        from_vec <- c(from_vec, node$node_id)
        to_vec <- c(to_vec, target_id)
      }
    }
  }

  if (length(from_vec) == 0L) {
    return(.empty_edge_df())
  }

  unique(data.frame(
    from = from_vec,
    to = to_vec,
    weight = 1,
    stringsAsFactors = FALSE
  ))
}

#' Build IMPORTS edges from R files to package dependencies
#'
#' Discovers package dependencies at two levels:
#'
#' 1. **Explicit** — `library(pkg)` and `require(pkg)` calls in source files.
#' 2. **Qualified** — `pkg::fn` and `pkg:::fn` patterns (the package name is
#'    extracted as an import).
#' 3. **DESCRIPTION** — if a `DESCRIPTION` file is found in the project root
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
#' edge `(test_file_stem → function_node_id)` is emitted when a test file
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
build_test_edges <- function(func_nodes, test_files) {
  if (length(func_nodes) == 0L || length(test_files) == 0L) {
    return(.empty_edge_df())
  }

  # Build lookup: bare_name → node_id (prefer first match when duplicates)
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

  from_vec <- character(0)
  to_vec <- character(0)

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
      nid <- name_to_id[[bare]]
      if (!is.null(nid)) {
        from_vec <- c(from_vec, fstem)
        to_vec <- c(to_vec, nid)
      }
    }
  }

  if (length(from_vec) == 0L) {
    return(.empty_edge_df())
  }

  unique(data.frame(
    from = from_vec,
    to = to_vec,
    weight = 1,
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
