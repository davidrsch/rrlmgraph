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
    head <- tryCatch(as.character(expr[[1L]]), error = function(e) "")

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
          as.character(expr[[3L]][[1L]]) == "R6Class",
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

  file_stem <- as.character(fs::path_ext_remove(fs::path_file(file_path)))
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

# ---- find_calls_in_body ---------------------------------------------

#' Find function calls within an R function body
#'
#' Uses `codetools::findGlobals()` to enumerate all global symbol references
#' in `fn_body`, then removes:
#' * Language primitives and operators.
#' * NSE false-positives: data-masked column names in tidyverse verbs,
#'   `.data$` pronouns, `{{ }}` injections, and `rlang::enquo()`/`sym()` args.
#'
#' For `pkg::fn` and `pkg:::fn` patterns the symbol is verified to exist as a
#' function in the package namespace before being included.
#'
#' @param fn_body An R language object (the body expression of a function,
#'   i.e.\ `body(f)` or the third element of a parsed `function(...)` call).
#'
#' @return A character vector of symbol names that represent real function
#'   calls, sorted and de-duplicated.  Returns `character(0)` on error.
#'
#' @seealso [extract_function_nodes()]
#' @export
#' @examples
#' fn <- function(x) sqrt(x) + log(x)
#' find_calls_in_body(body(fn))
#' # [1] "log"  "sqrt"
find_calls_in_body <- function(fn_body) {
  if (!is.call(fn_body) && !is.expression(fn_body) && !is.name(fn_body)) {
    return(character(0))
  }

  # Wrap in a dummy function so codetools can analyse the body.
  # MUST use pairlist() not list() for the formals -- R's function() primitive
  # expects a pairlist, not a plain list, as its first argument.
  dummy_fn <- tryCatch(
    eval(call("function", pairlist(), fn_body)),
    error = function(e) NULL
  )
  if (is.null(dummy_fn)) {
    return(character(0))
  }

  globals <- tryCatch(
    codetools::findGlobals(dummy_fn, merge = FALSE)$functions,
    error = function(e) character(0)
  )

  # Strip primitives / operators that are never meaningful CALLS edges
  primitives <- c(
    "{",
    "(",
    "<-",
    "=",
    "->",
    "<<-",
    "[",
    "[[",
    "$",
    "@",
    "+",
    "-",
    "*",
    "/",
    "^",
    "%%",
    "%/%",
    "%in%",
    "%>%",
    "|>",
    "==",
    "!=",
    "<",
    ">",
    "<=",
    ">=",
    "&",
    "|",
    "!",
    "&&",
    "||",
    ":",
    "~",
    "...",
    "..",
    ".GlobalEnv",
    "if",
    "for",
    "while",
    "repeat",
    "break",
    "next",
    "return",
    "function",
    "on.exit",
    "match.arg",
    "missing",
    "sys.call",
    "c",
    "list",
    "NULL",
    "TRUE",
    "FALSE",
    "Inf",
    "NaN",
    "NA",
    "NA_integer_",
    "NA_real_",
    "NA_complex_",
    "NA_character_",
    "vector",
    "character",
    "numeric",
    "logical",
    "integer",
    "complex",
    "raw",
    "environment",
    "new.env",
    "invisible"
  )
  globals <- setdiff(globals, primitives)

  # Collect NSE data-masked symbols to exclude
  nse_symbols <- .collect_nse_symbols(fn_body)
  globals <- globals[!globals %in% nse_symbols]

  # Remove remaining .data-like pronoun artefacts
  globals <- globals[!startsWith(globals, ".data")]

  # Handle pkg::fn and pkg:::fn -- walk AST directly for qualified calls
  qualified <- .find_qualified_calls(fn_body)

  # Remove the plain "::" / ":::" operator names; keep qualified refs
  globals <- globals[!globals %in% c("::", ":::")]
  globals <- union(globals, qualified)

  sort(unique(globals))
}

# ---- NSE helpers -----------------------------------------------------

#' @keywords internal
.collect_nse_symbols <- function(expr) {
  # Tidyverse verbs that data-mask their arguments
  nse_verbs <- c(
    "mutate",
    "filter",
    "summarise",
    "summarize",
    "select",
    "arrange",
    "group_by",
    "rename",
    "across",
    "where",
    "pick",
    "everything",
    "transmute",
    "slice",
    "case_when",
    "pull",
    "distinct",
    "count",
    "tally"
  )

  symbols <- character(0)

  walk <- function(e) {
    if (!is.call(e)) {
      return()
    }
    head <- tryCatch(as.character(e[[1L]]), error = function(x) "")
    bare_head <- sub("^.*:::", "", sub("^.*::", "", head))

    # NSE verb arguments
    if (bare_head %in% nse_verbs) {
      args <- as.list(e)[-1L]
      for (a in args) {
        if (is.name(a)) symbols <<- c(symbols, as.character(a))
      }
    }

    # .data$ pronoun
    if (head == "$" && length(e) == 3L) {
      lhs <- tryCatch(as.character(e[[2L]]), error = function(x) "")
      if (lhs == ".data") {
        rhs <- tryCatch(as.character(e[[3L]]), error = function(x) character(0))
        symbols <<- c(symbols, rhs)
      }
    }

    # {{ curly-curly }}
    if (head == "{{" && length(e) == 2L) {
      symbols <<- c(
        symbols,
        tryCatch(as.character(e[[2L]]), error = function(x) character(0))
      )
    }

    # rlang injection helpers
    bare_head_full <- sub("^rlang::", "", head)
    if (
      bare_head_full %in%
        c("enquo", "sym", "as_name", "ensym") &&
        length(e) >= 2L
    ) {
      symbols <<- c(
        symbols,
        tryCatch(as.character(e[[2L]]), error = function(x) character(0))
      )
    }

    lapply(as.list(e)[-1L], walk)
  }

  walk(expr)
  unique(symbols)
}

#' @keywords internal
.find_qualified_calls <- function(expr) {
  results <- character(0)

  walk <- function(e) {
    if (!is.call(e)) {
      return()
    }
    head <- tryCatch(as.character(e[[1L]]), error = function(x) "")

    if (head %in% c("::", ":::") && length(e) == 3L) {
      pkg_name <- tryCatch(as.character(e[[2L]]), error = function(x) NULL)
      fn_name <- tryCatch(as.character(e[[3L]]), error = function(x) NULL)

      if (!is.null(pkg_name) && !is.null(fn_name)) {
        exists_ok <- tryCatch(
          {
            ns <- tryCatch(asNamespace(pkg_name), error = function(x) NULL)
            if (!is.null(ns)) {
              exists(fn_name, where = ns, mode = "function", inherits = FALSE)
            } else {
              TRUE # namespace unavailable at index time -- keep tentatively
            }
          },
          error = function(x) TRUE
        )

        if (exists_ok) {
          results <<- c(results, paste0(pkg_name, head, fn_name))
        }
      }
    }

    lapply(as.list(e)[-1L], walk)
  }

  walk(expr)
  unique(results)
}
