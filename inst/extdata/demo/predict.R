#' Generate predictions from a fitted model
#'
#' Thin wrapper around \code{\link[stats]{predict.lm}} that validates that
#' \code{newdata} has the expected structure.
#'
#' @param model A fitted \code{lm} object, as returned by
#'   \code{\link{fit_model}}.
#' @param newdata A data.frame with predictor columns matching those used
#'   during training.
#' @return A named numeric vector of predictions (one element per row of
#'   \code{newdata}).
#' @seealso \code{\link{fit_model}}, \code{\link{evaluate_predictions}}
#' @export
predict_results <- function(model, newdata) {
  newdata <- clean_data(newdata)
  stats::predict(model, newdata = newdata)
}

#' Compute regression evaluation metrics
#'
#' Returns RMSE and R-squared for a vector of predictions vs.\ actuals.
#'
#' @param preds Numeric vector of predicted values.
#' @param actuals Numeric vector of actual values (same length as \code{preds}).
#' @return A named numeric vector with elements \code{rmse} and \code{rsq}.
#' @export
evaluate_predictions <- function(preds, actuals) {
  residuals <- actuals - preds
  rmse <- sqrt(mean(residuals^2))
  ss_res <- sum(residuals^2)
  ss_tot <- sum((actuals - mean(actuals))^2)
  rsq <- if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot
  c(rmse = rmse, rsq = rsq)
}

#' Run the full modelling pipeline
#'
#' Convenience wrapper that prepares data, fits a model, generates
#' predictions, and evaluates them in a single call.
#'
#' @param raw A data.frame with columns \code{x} and \code{y}.
#' @param ... Additional arguments forwarded to \code{\link{fit_model}}.
#' @return A list with elements \code{model} (the fitted \code{lm}),
#'   \code{predictions} (numeric vector), and \code{metrics} (named numeric
#'   vector from \code{\link{evaluate_predictions}}).
#' @seealso \code{\link{prepare_data}}, \code{\link{fit_model}},
#'   \code{\link{predict_results}}, \code{\link{evaluate_predictions}}
#' @export
run_pipeline <- function(raw, ...) {
  df <- prepare_data(raw)
  model <- fit_model(raw, ...)
  preds <- predict_results(model, df)
  metrics <- evaluate_predictions(preds, df$y)
  list(model = model, predictions = preds, metrics = metrics)
}
