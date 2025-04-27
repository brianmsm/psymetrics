#' Generic Plot Function for Model Visualizations
#'
#' @name plot-methods
#'
#' @description
#' `plot()` is a generic function designed for visualizing various aspects of
#' statistical models. It provides a unified interface for generating plots that
#' vary depending on the model type and the specific method implemented for that
#' model class.
#'
#' @details
#' Current available method:
#' - **Confirmatory Factor Analysis**: [plot.lavaan()] for plotting factor loadings and other metrics for models from the `lavaan` package.
#'
#' Future methods may include support for:
#' - **Exploratory Factor Analysis**: Models from the `psych` package.
#' - **Item Response Theory (IRT)**: Models from the `mirt` package.
#'
#' @return A plot object, usually a ggplot, specific to the model type.
#' @seealso
#'  [plot.lavaan()] for lavaan objects
#' @keywords internal
#'
NULL
