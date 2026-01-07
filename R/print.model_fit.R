#' Print Method for Model Fit Indices
#'
#' @description
#' `print.model_fit()` prints a text summary of model fit tables
#' to the console. By default, `Chi2` is shown with 2 decimals while
#' other indices use `digits`. For markdown or HTML output (or to
#' override column-specific digits), use [`format_results()`].
#'
#' @param x An object of class `model_fit`, typically created
#'   by the [`model_fit`] function.
#' @param digits Number of digits for rounding numeric values.
#'   Default is 3.
#' @param ci_digits Number of digits for rounding confidence
#'   intervals. Default is `digits`.
#' @param p_digits Number of digits for rounding p-values.
#'   Default is 3.
#' @param ... Other arguments are ignored.
#'
#' @return Returns `x` invisibly.
#' @exportS3Method base::print model_fit
#' @seealso [format_results()]
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'
#'   hs_model <- 'visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9'
#'
#'   fit <- cfa(hs_model, data = HolzingerSwineford1939,
#'              estimator = "MLR")
#'   result <- model_fit(fit)
#'   result
#'   print(result, digits = 4)
#'   if (requireNamespace("knitr", quietly = TRUE)) {
#'     format_results(result, output = "markdown")
#'   }
#' } else {
#'   message("Please install 'lavaan' to run this example.")
#' }
print.model_fit <- function(x, digits = 3, ci_digits = digits, p_digits = 3, ...) {
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
