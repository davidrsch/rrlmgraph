#' Validate raw input data
#'
#' Checks that required columns are present and that numeric columns contain
#' no non-finite values.
#'
#' @param raw A data.frame.
#' @param required_cols Character vector of required column names.
#' @return `raw` invisibly if valid; stops with an informative message otherwise.
#' @export
validate_inputs <- function(raw, required_cols = c("x", "y")) {
  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols)) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  num_cols <- names(raw)[vapply(raw, is.numeric, logical(1L))]
  bad <- num_cols[vapply(
    num_cols,
    function(cn) any(!is.finite(raw[[cn]])),
    logical(1L)
  )]
  if (length(bad)) {
    warning("Non-finite values found in: ", paste(bad, collapse = ", "))
  }
  invisible(raw)
}

#' Remove incomplete and duplicate rows
#'
#' @param raw A data.frame.
#' @return A cleaned data.frame with complete, deduplicated rows.
#' @export
clean_data <- function(raw) {
  raw <- raw[stats::complete.cases(raw), , drop = FALSE]
  raw <- unique(raw)
  raw
}

#' Prepare a data.frame for modelling
#'
#' Validates and cleans the raw input, then returns a modelling-ready
#' data.frame.
#'
#' @param raw A data.frame with at minimum columns \code{x} and \code{y}.
#' @param scale_x Logical.  Centre and scale the \code{x} column.  Default
#'   \code{FALSE}.
#' @return A cleaned, optionally scaled data.frame.
#' @seealso \code{\link{fit_model}}
#' @export
prepare_data <- function(raw, scale_x = FALSE) {
  validate_inputs(raw)
  df <- clean_data(raw)
  if (scale_x) {
    df$x <- as.numeric(scale(df$x))
  }
  df
}
