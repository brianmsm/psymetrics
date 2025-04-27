#' Compare Model Fit Indices Across Multiple Models
#'
#' @description `compare_model_fit()` compares the fit indices of two or more
#' models. It extracts the fit indices using [`model_fit()`] and combines them into
#' a single data frame for easy comparison.
#'
#' @param ... Two or more model objects to be compared.
#' @param type A character string specifying the type of fit indices to extract.
#'   Options are `"standard"`, `"scaled"`, and `"robust"`. Defaults to `NULL`,
#'   which automatically selects `"scaled"` if a robust estimator is used, otherwise `"standard"`.
#' @param metrics A character vector specifying which fit indices to extract.
#'   Defaults to `"essential"`, or a custom vector of indices.
#' @param verbose Logical. If `TRUE`, prints messages about the indices being adjusted.
#' @return A data frame containing the fit indices for each model, with an additional column identifying the models.
#'
#' @seealso
#'   [model_fit] for an overview of model fit methods in the package.
#'
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'   model1 <- 'visual  =~ x1 + x2 + x3 + x4'
#'   model2 <- 'visual  =~ x1 + x2 + x3 + x4 + x5'
#'   fit1 <- cfa(model1, data = HolzingerSwineford1939, estimator = "MLR")
#'   fit2 <- cfa(model2, data = HolzingerSwineford1939, estimator = "MLR")
#'   compare_model_fit(fit1, fit2)
#' } else {
#'   message("Please install 'lavaan' to run this example.")
#' }
#'
#' @export
compare_model_fit <- function(..., type = NULL, metrics = "essential", verbose = TRUE) {
  # Capture all the fit objects as a list
  fits <- list(...)

  # Ensure at least two models are provided for comparison
  if (length(fits) < 2) {
    cli::cli_abort(
      c("At least two model fits must be provided for comparison")
    )
  }

  # Apply model_fit to each model in the list
  fit_measures <- lapply(fits, model_fit, type = type,
                         metrics = metrics, verbose = verbose)

  # Combine the dataframes vertically
  combined_measures <- do.call(rbind, fit_measures)

  # Add a column to identify each model
  model_names <- sapply(substitute(list(...))[-1L], deparse)
  combined_measures$model <- rep(model_names, times = sapply(fit_measures, nrow))

  # Reorder columns so that "model" is the first column
  combined_measures <- combined_measures[, c("model", setdiff(names(combined_measures), "model"))]

  # Upper Text
  names(combined_measures)[1] <- "MODEL"

  # Assign the custom class for print method
  class(combined_measures) <- c("compare_model_fit", class(combined_measures))

  return(combined_measures)
}
