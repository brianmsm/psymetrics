#' Plot Method for lavaan Objects
#'
#' @description This is a plot method for objects of class `lavaan`. It allows users to create different types of plots to visualize key aspects of `lavaan` models, including factor loadings, residuals, and path diagrams.
#'
#' @param x A `lavaan` model object.
#' @param type A character string indicating the type of plot to generate. Options are:
#'   - `"factor_loadings"`: Generates a dot plot of standardized factor loadings, optionally including confidence intervals.
#'   - `"residuals"`: Generates a residual plot to visualize the differences between observed and model-implied covariances.
#'   - `"path"`: Generates a path diagram representing the model structure, showing the relationships between latent and observed variables.
#' @param standardized Logical; if `TRUE`, uses standardized estimates for factor loadings. Only applicable when `type = "factor_loadings"`. Defaults to `TRUE`.
#' @param CI Logical; if `TRUE`, includes confidence intervals in the factor loading plot. Only applicable when `type = "factor_loadings"`. Defaults to `TRUE`.
#' @param ... Additional arguments passed to the specific plotting functions.
#'
#' @return A ggplot object for `factor_loadings` and `residuals` plots if `ggplot2` is installed, or a `semPlot` diagram object for `path` plots. An error message will be returned if required packages are not available.
#'
#' @details
#' - **Factor Loadings Plot**: Displays a dot plot of factor loadings, with items on the y-axis and loadings on the x-axis. Confidence intervals can be added if desired.
#' - **Residuals Plot**: Shows the residuals (differences between observed and model-implied covariances), typically as a heatmap or scatterplot.
#' - **Path Diagram**: Illustrates the structure of the model, showing latent variables, observed variables, and the estimated relationships between them.
#'
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'   HS.model <- ' visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9 '
#'   fit <- cfa(HS.model, data = HolzingerSwineford1939)
#'   plot(fit)
#' } else {
#'   message("Please install 'lavaan' and 'ggplot2' to run this example.")
#' }
#'
#' @seealso
#'   [plot-methods] for an overview of plotting in the package.
#'   [plot_factor_loadings()] which is called by this method for type = "factor_loadings".
#'
#' @exportS3Method graphics::plot lavaan
plot.lavaan <- function(x, type = "factor_loadings", standardized = TRUE, CI = TRUE, ...) {
  rlang::check_installed("lavaan", reason = "to process 'lavaan' objects.")
  if (!inherits(x, "lavaan")) {
    cli::cli_abort("The object is not a valid lavaan model.")
  }

  if (type == "factor_loadings") {
    plot_factor_loadings(x, standardized = standardized, CI = CI, ...)
  } else if (type == "residuals") {
    cli::cli_inform("Plotting residuals is not yet implemented.")
  } else if (type == "path") {
    cli::cli_inform("Plotting path diagrams is not yet implemented.")
  } else if (type == "comparison") {
    cli::cli_inform("Model comparison plotting is not yet implemented.")
  } else {
    cli::cli_abort("Unsupported plot type for lavaan model.")
  }
}


#' Plot Factor Loadings for lavaan Models
#'
#' @description Creates a dot plot of standardized factor loadings for a `lavaan` model. The plot shows factor loadings along with optional confidence intervals for each item in the model.
#'
#' @param fit A `lavaan` model object.
#' @param sort Logical; if `TRUE`, sorts items by loading size. Defaults to `TRUE`.
#' @param group_by Logical; if `TRUE` and the model has multiple factors, groups the items by factor. Defaults to `TRUE`.
#' @param standardized Logical; if `TRUE`, uses standardized loadings. Defaults to `TRUE`.
#' @param CI Logical; if `TRUE`, includes confidence intervals. Defaults to `TRUE`.
#' @param autofit Logical; if `TRUE`, adjusts the x-axis range based on the range of loadings. Defaults to `TRUE`.
#' @param ... Additional arguments passed to `ggplot2::ggplot`.
#'
#' @return A ggplot object if `ggplot2` is installed, otherwise an error message.
#' @importFrom rlang .data
#'
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'   HS.model <- ' visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9 '
#'   fit <- cfa(HS.model, data = HolzingerSwineford1939)
#'   plot_factor_loadings(fit)
#' } else {
#'   message("Please install 'lavaan' and 'ggplot2' to run this example.")
#' }
#'
#' #' @seealso
#'   [plot-methods] for an overview of plotting in the package.
#'   [plot.lavaan()] for more lavaan object plots
#' @export
plot_factor_loadings <- function(fit, sort = TRUE, group_by = TRUE, standardized = TRUE, CI = TRUE, autofit = TRUE, ...) {
  rlang::check_installed("lavaan", reason = "to process 'lavaan' objects.")
  rlang::check_installed("ggplot2", reason = "to create dot plots for factor loadings")

  # Extract standardized loadings and confidence intervals
  if (standardized) {
    loadings <- lavaan::standardizedSolution(fit)
    names(loadings)[4] <- "est"
  } else {
    loadings <- lavaan::parameterEstimates(fit)
  }

  # Filter to retain only factor loadings (lambda parameters)
  loadings <- loadings[loadings$op == "=~", ]
  loadings <- loadings[, c("lhs", "rhs", "est", "ci.lower", "ci.upper")]

  # Set the order of `rhs` based on `sort`
  loadings$rhs <- if (sort) {
    factor(loadings$rhs, levels = levels(stats::reorder(loadings$rhs, loadings$est)))
  } else {
    factor(loadings$rhs)
  }

  # Adjust x-axis range if autofit is TRUE
  x_limits <- if (autofit & standardized) {
    c(NA, 1)
  } else if (standardized) {
    c(0, 1)
  } else {
    c(NA, NA)
  }

  # Grouping by factors if multiple factors are present and group_by is TRUE
  if (group_by) {
    loadings$Factor <- factor(loadings$lhs, levels = unique(loadings$lhs))
  } else {
    loadings$Factor <- "All Items"
  }

  # Plot with .data to avoid global variable warnings
  plot <- ggplot2::ggplot(loadings, ggplot2::aes(x = .data$est, y = .data$rhs)) +
    ggplot2::geom_point(size = 3, ggplot2::aes(color = .data$Factor)) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::labs(
      x = "Factor Loadings",
      y = "Items",
      title = "Factor Loadings"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(limits = x_limits) +
    ggplot2::theme(legend.position = "bottom")

  # Add confidence intervals if CI is TRUE
  if (CI) {
    plot <- plot + ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$ci.lower, xmax = .data$ci.upper),
      height = 0.2, color = "grey50"
    )
  }

  plot
}
