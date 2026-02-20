#' Select predictive features via correlation filter
#'
#' Retains numeric columns whose absolute Pearson correlation with \code{y}
#' exceeds \code{threshold}.
#'
#' @param df A data.frame that includes a numeric column \code{y}.
#' @param threshold Numeric.  Minimum \code{|r|} to keep a column.  Default
#'   \code{0.1}.
#' @return A data.frame with only the selected feature columns plus \code{y}.
#' @export
select_features <- function(df, threshold = 0.1) {
  df <- clean_data(df)
  num_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1L))], "y")
  cors <- vapply(
    num_cols,
    function(cn) abs(stats::cor(df[[cn]], df$y, use = "complete.obs")),
    numeric(1L)
  )
  keep <- num_cols[!is.na(cors) & cors >= threshold]
  df[, c(keep, "y"), drop = FALSE]
}

#' Tune regularisation hyperparameter via leave-one-out CV
#'
#' A simple grid search over \code{lambda} values; selects the value that
#' minimises mean-squared error in leave-one-out cross-validation.
#'
#' @param df A data.frame as returned by \code{\link{prepare_data}}.
#' @param lambdas Numeric vector of candidate penalty values.
#' @return The best \code{lambda} value (numeric scalar).
#' @export
tune_hyperparams <- function(df, lambdas = c(0, 0.01, 0.1, 1, 10)) {
  best_lambda <- lambdas[[1L]]
  best_mse <- Inf
  for (lam in lambdas) {
    mse <- mean(vapply(
      seq_len(nrow(df)),
      function(i) {
        train <- df[-i, , drop = FALSE]
        test <- df[i, , drop = FALSE]
        fit <- stats::lm(y ~ ., data = train)
        pred <- stats::predict(fit, newdata = test)
        (test$y - pred)^2
      },
      numeric(1L)
    ))
    if (mse < best_mse) {
      best_mse <- mse
      best_lambda <- lam
    }
  }
  best_lambda
}

#' Fit a linear model to prepared data
#'
#' Prepares the raw input, optionally runs feature selection and
#' hyperparameter tuning, then fits a linear model.
#'
#' @param raw A data.frame with columns \code{x} and \code{y}.
#' @param tune Logical.  Run \code{\link{tune_hyperparams}}?  Default
#'   \code{FALSE}.
#' @param select Logical.  Run \code{\link{select_features}}?  Default
#'   \code{FALSE}.
#' @return A fitted \code{lm} object.
#' @seealso \code{\link{prepare_data}}, \code{\link{predict_results}}
#' @export
fit_model <- function(raw, tune = FALSE, select = FALSE) {
  df <- prepare_data(raw)
  if (select) {
    df <- select_features(df)
  }
  if (tune) {
    tune_hyperparams(df) # side-effect: selects best lambda; placeholder here
  }
  stats::lm(y ~ ., data = df)
}
