#' Plot fit indices from `psymetrics` fit objects
#'
#' @description
#' `plot_model_fit()` is the public plotting entrypoint for fit-index
#' objects created by [model_fit()] and [compare_model_fit()].
#' In `v0.5.0`, it supports exactly two classes:
#' - `model_fit` for a single fitted model
#' - `compare_model_fit` for two or more fitted models
#'
#' The default plot style is chosen automatically from the input class.
#' For `model_fit`, the default is a single-fit bullet chart. For
#' `compare_model_fit`, the default is a threshold-aware dot plot.
#'
#' @param x A `model_fit` or `compare_model_fit` object.
#' @param type Character string indicating the plot style.
#'   Supported values depend on the input class:
#'   - `model_fit`: `"default"`, `"bullet"`
#'   - `compare_model_fit`: `"default"`, `"dots"`, `"bars"`, `"heatmap"`
#' @param metrics Optional character vector selecting which fit indices to plot.
#'   Supported metrics are `"CFI"`, `"TLI"`, `"RMSEA"`, and `"SRMR"`.
#'   Defaults to `NULL`, which uses those metrics in canonical order when they
#'   are present in the object.
#' @param verbose Logical. If `TRUE`, non-fatal informational messages are shown
#'   when requested metrics are unavailable and dropped.
#' @param ... Reserved for future extensions. Currently ignored.
#'
#' @return A `ggplot` object.
#' @importFrom rlang .data
#' @seealso [model_fit()], [compare_model_fit()], and [plot_factor_loadings()].
#' @export
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'
#'   hs_model <- 'visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9'
#'
#'   fit_mlr <- cfa(hs_model, data = HolzingerSwineford1939, estimator = "MLR")
#'   fit_ulsm <- cfa(hs_model, data = HolzingerSwineford1939, estimator = "ULSM")
#'
#'   single_fit <- model_fit(fit_mlr)
#'   plot_model_fit(single_fit)
#'
#'   compared_fits <- compare_model_fit(MLR = fit_mlr, ULSM = fit_ulsm)
#'   plot_model_fit(compared_fits)
#'   plot_model_fit(compared_fits, type = "bars")
#' }
plot_model_fit <- function(x, type = "default", metrics = NULL, verbose = TRUE, ...) {
  UseMethod("plot_model_fit")
}

#' @export
plot_model_fit.default <- function(x, type = "default", metrics = NULL, verbose = TRUE, ...) {
  plot_model_fit_validate_input(x)
}

