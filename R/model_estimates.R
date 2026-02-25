#' Generic Function to Extract Model Parameter Estimates
#'
#' @description
#' `model_estimates()` is a generic function that extracts
#' model parameter estimates from supported model objects.
#' It provides a consistent interface for CFA/SEM parameter
#' extraction and dispatches to class-specific methods.
#'
#' @param fit A fitted model object.
#' @param ... Additional arguments passed to class-specific methods.
#'
#' @return A data frame with parameter estimates. For supported
#'   classes, the result includes class `model_estimates`.
#' @seealso [model_estimates.lavaan()] for `lavaan` objects and
#'   [model_fit()] for fit-index extraction.
#' @export
model_estimates <- function(fit, ...) {
  UseMethod("model_estimates")
}

#' @export
model_estimates.default <- function(fit, ...) {
  fit_class <- class(fit)
  if (is.null(fit_class) || length(fit_class) == 0L) {
    fit_class <- typeof(fit)
  }
  fit_class <- paste(fit_class, collapse = ", ")

  cli::cli_abort(c(
    sprintf("Objects of class '%s' are not currently supported by `model_estimates()`.", fit_class),
    "Supported classes: lavaan."
  ))
}
