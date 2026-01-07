#' Print Method for compare_model_fit Objects
#'
#' @description
#' `print.compare_model_fit()` prints a text summary of
#' compare-model-fit tables to the console. By default, `Chi2`
#' is shown with 2 decimals while other indices use `digits`.
#' For markdown or HTML output (or to override column-specific
#' digits), use [`format_results()`].
#'
#' @param x An object of class `compare_model_fit`, typically
#'   created by the [`compare_model_fit`] function.
#' @param digits Number of digits for rounding numeric values.
#'   Default is 3.
#' @param ci_digits Number of digits for rounding confidence
#'   intervals. Default is `digits`.
#' @param p_digits Number of digits for rounding p-values.
#'   Default is 3.
#' @param ... Other arguments are ignored.
#'
#' @return Returns `x` invisibly.
#' @exportS3Method base::print compare_model_fit
#' @seealso [format_results()]
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   library(psymetrics)
#'   library(lavaan)
#'
#'   hs_model <- 'visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9'
#'   fit1 <- cfa(hs_model, data = HolzingerSwineford1939,
#'               estimator = "ML")
#'   fit2 <- cfa(hs_model, data = HolzingerSwineford1939,
#'               estimator = "MLR")
#'   comparison <- compare_model_fit(fit1, fit2)
#'   comparison
#'   print(comparison, digits = 4)
#'   format_results(comparison, output = "html")
#' } else {
#'   message("Please install 'lavaan' to run this example.")
#' }
print.compare_model_fit <- function(x, digits = 3, ci_digits = digits, p_digits = 3, ...) {
  txt <- format_results(
    x,
    output = "text",
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits
  )
  cat(txt, sep = "\n")
  invisible(x)
}
