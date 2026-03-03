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
    head <- tryCatch(as.character(e[[1L]])[[1L]], error = function(x) "")
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
    head <- tryCatch(as.character(e[[1L]])[[1L]], error = function(x) "")

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

    # Walk all elements including e[[1L]] so that pkg::fn(arg) is found
    lapply(as.list(e), walk)
  }

  walk(expr)
  unique(results)
}
