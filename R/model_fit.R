#' Generic Function to Extract Model Fit Indices
#'
#' @description
#' `model_fit()` is a generic function designed to extract fit indices from a
#' variety of statistical model objects. It serves as a unified interface for
#' obtaining key fit metrics, which can vary depending on the type of model and
#' the specific method implemented for that model class.
#'
#' @param fit A model object from which to extract fit indices. The class of this
#'   object determines the specific method that will be used. For example, objects
#'   of class `lavaan` will use the method [`model_fit.lavaan()`].
#' @param ... Additional arguments passed to the specific method for the model class.
#'
#' @return A `data.frame` containing the fit indices of the model. The specific indices
#'   returned depend on the model type and the method used.
#'
#' @details
#' The `model_fit()` function is intended to provide a consistent interface for
#' extracting fit indices from various types of models. Methods for specific
#' classes of models should implement the necessary logic to retrieve and return
#' relevant fit indices in a tidy format.
#'
#' **Methods currently implemented:**
#'
#' - `lavaan`: For models estimated using the `lavaan` package, use the
#'   [`model_fit.lavaan()`] method.
#' - `psych` (Future): Future methods may include support for models from the
#'   `psych` package.
#' - `mirt` (Future): Future methods may include support for models from the
#'   `mirt` package.
#'
#' @export
model_fit <- function(fit, ...) {
  UseMethod("model_fit")
}
