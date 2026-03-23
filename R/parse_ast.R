#' Extract function nodes from R source files
#'
#' Parses every file in `r_files`, walks its AST, and returns a list of node
#' records -- one per named function definition discovered.  Handles standard
#' `<-`/`=` assignments, S4 generics/methods/classes, and R6 class methods.
#' Files that cannot be parsed are skipped with a warning; the function never
#' throws.
#'
#' @param r_files Character vector of absolute paths to `.R` source files
#'   (typically the `r_files` element returned by [detect_rproject()]).
#'
#' @return A list of node records.  Each record is a named list with elements:
#'   \describe{
#'     \item{`node_id`}{`"<file_stem>::<fn_name>"` character(1).}
#'     \item{`name`}{Function name, character(1).}
#'     \item{`file`}{Absolute path of the source file, character(1).}
#'     \item{`line_start`}{Integer: first line of the definition.}
#'     \item{`line_end`}{Integer: last line of the definition.}
#'     \item{`signature`}{Deparsed signature: `"name(arg1, arg2)"`, character(1).}
#'     \item{`body_text`}{Full source text of the definition, character(1).}
#'     \item{`roxygen_text`}{Adjacent Roxygen block (may be `""`), character(1).}
#'     \item{`complexity`}{Integer: approximate cyclomatic complexity.}
#'     \item{`calls_list`}{Character vector of called symbol names.}
#'   }
#'
#' @seealso [find_calls_in_body()]
#' @export
#' @examples
#' \dontrun{
#' proj  <- detect_rproject("/path/to/mypkg")
#' nodes <- extract_function_nodes(proj$r_files)
#' length(nodes)  # number of functions found
#' }
extract_function_nodes <- function(r_files) {
  nodes <- list()
  for (fpath in r_files) {
    src <- tryCatch(
      readLines(fpath, warn = FALSE),
      error = function(e) {
        cli::cli_warn("Cannot read {.path {fpath}}: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(src)) {
      next
    }

    parsed <- tryCatch(
      parse(fpath, keep.source = TRUE),
      error = function(e) {
        cli::cli_warn("Parse error in {.path {fpath}}: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(parsed)) {
      next
    }

    exprs <- as.list(parsed)
    # parse() stores srcrefs as an attribute on the whole result object, NOT
    # on individual expressions.  Using lapply(exprs, getSrcref) returns all
    # NULLs, causing every node to be silently dropped in .make_node().
    src_refs <- attr(parsed, "srcref")
    if (is.null(src_refs)) {
      src_refs <- vector("list", length(exprs))
    }
    new_nodes <- .extract_from_exprs(exprs, src_refs, src, fpath)
    nodes <- c(nodes, new_nodes)
  }
  nodes
}

# ---- private AST walker ---------------------------------------------

#' @keywords internal
.extract_from_exprs <- function(exprs, src_refs, source_lines, file_path) {
  nodes <- list()

  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    sr <- src_refs[[i]]

    if (!is.call(expr)) {
      next
    }
    head <- tryCatch(as.character(expr[[1L]])[[1L]], error = function(e) "")

    # ---- Standard assignment: `name <- function(args) body`  ---------
    if (
      head %in%
        c("<-", "=", "->") &&
        length(expr) == 3L &&
        is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name("function"))
    ) {
      fn_name <- tryCatch(as.character(expr[[2L]]), error = function(e) NULL)
      if (!is.null(fn_name)) {
        nd <- .make_node(fn_name, expr[[3L]], sr, source_lines, file_path)
        if (!is.null(nd)) nodes <- c(nodes, list(nd))
      }

      # ---- S4 constructs ---------------------------------------------------
    } else if (head %in% c("setGeneric", "setMethod", "setClass")) {
      fn_name <- tryCatch(as.character(expr[[2L]]), error = function(e) NULL)
      if (!is.null(fn_name)) {
        nd <- .make_node(
          fn_name,
          expr,
          sr,
          source_lines,
          file_path,
          kind = head
        )
        if (!is.null(nd)) nodes <- c(nodes, list(nd))
      }

      # ---- R6 class --------------------------------------------------------
    } else if (
      length(expr) == 3L &&
        is.call(expr[[3L]]) &&
        tryCatch(
          "R6Class" %in% as.character(expr[[3L]][[1L]]),
          error = function(e) FALSE
        )
    ) {
      r6_nodes <- .extract_r6_methods(expr, sr, source_lines, file_path)
      nodes <- c(nodes, r6_nodes)
    }
  }
  nodes
}

#' @keywords internal
.make_node <- function(
  fn_name,
  fn_expr,
  src_ref,
  source_lines,
  file_path,
  kind = "function"
) {
  if (is.null(src_ref)) {
    return(NULL)
  }

  line_start <- as.integer(src_ref[1L])
  line_end <- as.integer(src_ref[3L])

  # Body text: source lines covering the definition
  max_line <- length(source_lines)
  body_text <- paste(
    source_lines[seq(line_start, min(line_end, max_line))],
    collapse = "\n"
  )

  # Signature: reconstruct from formals when possible
  signature <- tryCatch(
    {
      if (is.call(fn_expr) && identical(fn_expr[[1L]], as.name("function"))) {
        fmls <- fn_expr[[2L]]
        args <- if (is.null(fmls) || length(fmls) == 0L) {
          ""
        } else {
          paste(names(fmls), collapse = ", ")
        }
        paste0(fn_name, "(", args, ")")
      } else {
        fn_name
      }
    },
    error = function(e) fn_name
  )

  # Roxygen: scan backward from line_start - 1
  roxygen_text <- .scrape_roxygen(source_lines, line_start)

  # Cyclomatic complexity approximation
  complexity <- .approx_complexity(body_text)

  # Calls: delegate to find_calls_in_body
  calls_list <- tryCatch(
    {
      body_expr <- if (
        is.call(fn_expr) &&
          identical(fn_expr[[1L]], as.name("function")) &&
          length(fn_expr) >= 3L
      ) {
        fn_expr[[3L]]
      } else {
        fn_expr
      }
      find_calls_in_body(body_expr)
    },
    error = function(e) character(0)
  )

  # Include the immediate parent directory in the stem to disambiguate
  # files with the same name in different directories (e.g. R/utils.R vs
  # tests/utils.R), which would otherwise produce colliding node_ids.
  dir_part <- basename(dirname(as.character(file_path)))
  stem_part <- as.character(fs::path_ext_remove(fs::path_file(file_path)))
  file_stem <- if (nzchar(dir_part) && dir_part != ".") {
    paste0(dir_part, "/", stem_part)
  } else {
    stem_part
  }
  node_id <- paste0(file_stem, "::", fn_name)

  list(
    node_id = node_id,
    name = fn_name,
    file = as.character(file_path),
    line_start = line_start,
    line_end = line_end,
    signature = signature,
    body_text = body_text,
    roxygen_text = roxygen_text,
    complexity = as.integer(complexity),
    calls_list = calls_list
  )
}

#' @keywords internal
.scrape_roxygen <- function(source_lines, line_start) {
  if (line_start <= 1L) {
    return("")
  }
  idx <- line_start - 1L
  lines <- character(0)
  while (idx >= 1L && grepl("^\\s*#'", source_lines[[idx]])) {
    lines <- c(source_lines[[idx]], lines)
    idx <- idx - 1L
  }
  paste(lines, collapse = "\n")
}

#' @keywords internal
.approx_complexity <- function(body_text) {
  kw_re <- "\\b(if|for|while|repeat|tryCatch|switch)\\b"
  matches <- gregexpr(kw_re, body_text, perl = TRUE)[[1L]]
  n_kw <- if (identical(matches, -1L)) 0L else length(matches)
  1L + n_kw
}

#' @keywords internal
.extract_r6_methods <- function(assign_expr, src_ref, source_lines, file_path) {
  nodes <- list()
  class_name <- tryCatch(as.character(assign_expr[[2L]]), error = function(e) {
    "R6Class"
  })
  r6_call <- assign_expr[[3L]] # R6Class("Name", ...)

  # Named arguments after the class name
  args <- as.list(r6_call)[-1L]

  for (section in c("public", "private", "active")) {
    members <- args[[section]]
    if (is.null(members)) {
      next
    }

    # Unwrap `list(...)` if present
    if (
      is.call(members) &&
        identical(members[[1L]], as.name("list"))
    ) {
      member_list <- as.list(members)[-1L]
    } else {
      next
    }

    for (nm in names(member_list)) {
      val <- member_list[[nm]]
      if (is.call(val) && identical(val[[1L]], as.name("function"))) {
        fn_name <- paste0(class_name, "$", nm)
        nd <- .make_node(
          fn_name,
          val,
          src_ref,
          source_lines,
          file_path,
          kind = "R6method"
        )
        if (!is.null(nd)) nodes <- c(nodes, list(nd))
      }
    }
  }
  nodes
}
