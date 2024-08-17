#' Generic Function to Extract Model Fit Indices
#'
#' @description `model_fit()` is a generic function that extracts fit indices
#' from various types of model objects.
#'
#' @param model A model object. Specific methods exist for different model types.
#' @return A `tibble` containing the fit indices of the model.
#' @export
model_fit <- function(model) {
  UseMethod("model_fit")
}
