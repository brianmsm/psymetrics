#' Plot Method for lavaan Objects
#'
#' @description
#' This is a plot method for objects of class `lavaan`. It
#' allows users to create different types of plots to visualize
#' key aspects of `lavaan` models, including factor loadings,
#' residuals, and path diagrams.
#'
#' @param x A `lavaan` model object.
#' @param type A character string indicating the type of plot
#'   to generate. Options are:
#'   - `"factor_loadings"`: Generates a dot plot of standardized
#'      factor loadings, optionally including confidence intervals.
#'   - `"residuals"`: Generates a residual plot to visualize the
#'      differences between observed and model-implied covariances.
#'   - `"path"`: Generates a path diagram representing the model
#'      structure, showing the relationships between latent and
#'      observed variables.
#' @param standardized Logical; if `TRUE`, uses standardized
#'   estimates for factor loadings. Only applicable when
#'   `type = "factor_loadings"`. Defaults to `TRUE`.
#' @param ci Logical; if `TRUE`, includes confidence intervals
#'   in the factor loading plot. Only applicable when
#'   `type = "factor_loadings"`. Defaults to `TRUE`. Also accepts
#'   `CI` via `...` for backward compatibility.
#' @param ... Additional arguments passed to the specific
#'   plotting functions.
#'
#' @return A ggplot object for `factor_loadings` and `residuals`
#'   plots if `ggplot2` is installed, or a `semPlot` diagram
#'   object for `path` plots. An error message will be returned
#'   if required packages are not available.
#' @details
#'   - **Factor Loadings Plot**: Displays a dot plot of factor
#'     loadings, with items on the y-axis and loadings on the
#'     x-axis. Confidence intervals can be added if desired.
#'   - **Residuals Plot**: Shows the residuals (differences
#'     between observed and model-implied covariances), typically
#'     as a heatmap or scatterplot.
#'   - **Path Diagram**: Illustrates the structure of the model,
#'     showing latent variables, observed variables, and the
#'     estimated relationships between them.
#' @seealso
#'   - [plot-methods] for an overview of plotting in the package.
#'   - [plot_factor_loadings()] which is called by this method
#'     for type = "factor_loadings".
#' @exportS3Method graphics::plot lavaan
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'   hs_model <- 'visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9'
#'   fit <- cfa(hs_model, data = HolzingerSwineford1939, estimator = "MLR")
#'   plot(fit)
#' } else {
#'   message("Please install 'lavaan' and 'ggplot2' to run this example.")
#' }
plot.lavaan <- function(x, type = "factor_loadings", standardized = TRUE, ci = TRUE, ...) {
  rlang::check_installed("lavaan", reason = "to process 'lavaan' objects.")
  if (!inherits(x, "lavaan")) {
    cli::cli_abort("The object is not a valid lavaan model.")
  }
  dots <- list(...)
  if ("CI" %in% names(dots) && missing(ci)) {
    ci <- dots$CI
  }
  dots$CI <- NULL

  if (type == "factor_loadings") {
    do.call(
      plot_factor_loadings,
      c(list(fit = x, standardized = standardized, ci = ci), dots)
    )
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
#' @description
#' Creates a dot plot of standardized factor loadings
#' for a `lavaan` model. The plot shows factor loadings
#' along with optional confidence intervals for each item
#' in the model.
#'
#' @param fit A `lavaan` model object.
#' @param sort Logical; if `TRUE`, sorts items by loading
#'   size. Defaults to `TRUE`.
#' @param group_by Logical; if `TRUE` and the model has
#'   multiple factors, groups the items by factor. Defaults
#'   to `TRUE`.
#' @param standardized Logical; if `TRUE`, uses standardized
#'   loadings. Defaults to `TRUE`.
#' @param ci Logical; if `TRUE`, includes confidence intervals.
#'   Defaults to `TRUE`. Also accepts `CI` via `...` for backward
#'   compatibility.
#' @param autofit Logical; if `TRUE`, computes and applies
#'   x-axis limits via `coord_cartesian()` based on loadings
#'   (and CIs when available). If all values are non-negative,
#'   the lower limit is set to 0; if all values are non-positive,
#'   the upper limit is set to 0. For standardized loadings within
#'   `[-1, 1]`, the limits are extended to include the nearest
#'   boundary to keep the standardized scale visible. If `FALSE`,
#'   the x-axis limits are not modified and ggplot2 determines the range.
#' @param ci_bounds Controls how confidence intervals are handled
#'   for standardized loadings when `ci = TRUE` and `autofit = TRUE`.
#'   `"extend"` draws full CIs (axis may extend beyond `[-1, 1]`);
#'   `"arrow"` constrains the x-axis to `[-1, 1]` (or `[0, 1]`/`[-1, 0]`
#'   when all values are non-negative/non-positive), clips CIs to that
#'   range, and adds arrows to indicate off-scale intervals. If any
#'   standardized point estimate is outside `[-1, 1]`, `"arrow"`
#'   falls back to `"extend"`.
#' @param verbose Logical; if `TRUE`, prints informational messages
#'   and warnings (for example, when the model did not converge).
#'   Defaults to `TRUE`.
#' @param ... Additional arguments passed to `ggplot2::ggplot`.
#'
#' @return A ggplot object if `ggplot2` is installed, otherwise
#'   an error message.
#' @seealso
#'   - [plot-methods] for an overview of plotting in the package.
#'   - [plot.lavaan()] for more lavaan object plots.
#' @importFrom rlang .data
#' @export
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'   hs_model <- 'visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9'
#'   fit <- cfa(hs_model, data = HolzingerSwineford1939, estimator = "MLR")
#'   plot_factor_loadings(fit)
#' } else {
#'   message("Please install 'lavaan' and 'ggplot2' to run this example.")
#' }
plot_factor_loadings <- function(fit,
                                 sort = TRUE,
                                 group_by = TRUE,
                                 standardized = TRUE,
                                 ci = TRUE,
                                 autofit = TRUE,
                                 ci_bounds = c("extend", "arrow"),
                                 verbose = TRUE,
                                 ...) {
  rlang::check_installed("lavaan", reason = "to process 'lavaan' objects.")
  rlang::check_installed("ggplot2", reason = "to create dot plots for factor loadings")
  ci_bounds <- match.arg(ci_bounds)
  dots <- list(...)
  if ("CI" %in% names(dots) && missing(ci)) {
    ci <- dots$CI
  }
  dots$CI <- NULL
  converged <- tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) NA)
  if (isFALSE(converged) && isTRUE(verbose)) {
    cli::cli_warn("The model did not converge. Interpret the loadings with caution.")
  }

  # Extract standardized loadings and confidence intervals
  if (standardized) {
    loadings <- lavaan::standardizedSolution(fit)
    if ("est.std" %in% names(loadings)) {
      loadings$est <- loadings$est.std
    }
  } else {
    loadings <- lavaan::parameterEstimates(fit)
  }
  if (!"est" %in% names(loadings)) {
    cli::cli_abort("Could not find an estimate column (`est` or `est.std`) in the extracted lavaan loadings.")
  }

  # Filter to retain only factor loadings (lambda parameters)
  loadings <- loadings[loadings$op == "=~", ]
  required_cols <- c("lhs", "rhs", "est")
  optional_ci_cols <- c("ci.lower", "ci.upper")
  available_cols <- c(required_cols, intersect(optional_ci_cols, names(loadings)))
  loadings <- loadings[, available_cols, drop = FALSE]
  has_ci <- all(optional_ci_cols %in% names(loadings)) &&
    any(!is.na(c(loadings$ci.lower, loadings$ci.upper)))

  # Set the order of `rhs` based on `sort`
  loadings$rhs <- if (sort) {
    factor(loadings$rhs, levels = levels(stats::reorder(loadings$rhs, loadings$est)))
  } else {
    factor(loadings$rhs)
  }

  use_ci_bounds <- isTRUE(standardized) && isTRUE(ci) && isTRUE(autofit) && has_ci
  if (!use_ci_bounds && ci_bounds != "extend" && isTRUE(verbose)) {
    cli::cli_inform(
      "Ignoring `ci_bounds` because it only applies when `standardized = TRUE`, `ci = TRUE`, CIs are available, and `autofit = TRUE`."
    )
  }

  ci_bounds_effective <- if (use_ci_bounds) ci_bounds else "extend"
  if (use_ci_bounds && ci_bounds_effective == "arrow") {
    heywood <- any(loadings$est < -1 | loadings$est > 1, na.rm = TRUE)
    if (isTRUE(heywood)) {
      if (isTRUE(verbose)) {
        cli::cli_inform(
          "Standardized estimates outside [-1, 1]; using `ci_bounds = \"extend\"` instead of `\"arrow\"`."
        )
      }
      ci_bounds_effective <- "extend"
    }
  }

  x_limits <- NULL
  if (isTRUE(autofit)) {
    range_values <- loadings$est
    if (isTRUE(ci) && has_ci) {
      range_values <- c(range_values, loadings$ci.lower, loadings$ci.upper)
    }
    range_values <- range_values[is.finite(range_values)]
    if (length(range_values) > 0) {
      all_nonneg <- all(range_values >= 0)
      all_nonpos <- all(range_values <= 0)
      if (ci_bounds_effective == "arrow") {
        if (all_nonneg) {
          x_limits <- c(0, 1)
        } else if (all_nonpos) {
          x_limits <- c(-1, 0)
        } else {
          x_limits <- c(-1, 1)
        }
      } else {
        x_limits <- range(range_values, na.rm = TRUE)
        if (all_nonneg) {
          x_limits[1] <- 0
        }
        if (all_nonpos) {
          x_limits[2] <- 0
        }
        if (isTRUE(standardized) && all(range_values >= -1 & range_values <= 1)) {
          if (all_nonneg) {
            x_limits[2] <- max(x_limits[2], 1)
          } else if (all_nonpos) {
            x_limits[1] <- min(x_limits[1], -1)
          } else {
            x_limits[1] <- min(x_limits[1], -1)
            x_limits[2] <- max(x_limits[2], 1)
          }
        }
      }
    }
  }

  # Grouping by factors if multiple factors are present and group_by is TRUE
  if (group_by) {
    loadings$Factor <- factor(loadings$lhs, levels = unique(loadings$lhs))
  } else {
    loadings$Factor <- "All Items"
  }

  # Plot with .data to avoid global variable warnings
  plot <- do.call(
    ggplot2::ggplot,
    c(list(data = loadings, mapping = ggplot2::aes(x = .data$est, y = .data$rhs)), dots)
  ) +
    ggplot2::geom_point(size = 3, ggplot2::aes(color = .data$Factor)) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::labs(
      x = "Factor Loadings",
      y = "Items",
      title = "Factor Loadings"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  if (isTRUE(autofit) && !is.null(x_limits)) {
    plot <- plot + ggplot2::coord_cartesian(xlim = x_limits)
  }

  # Add confidence intervals if ci is TRUE and available
  if (isTRUE(ci) && !has_ci && isTRUE(verbose)) {
    cli::cli_inform("Confidence intervals are unavailable for this model; skipping error bars.")
  }
  if (isTRUE(ci) && has_ci) {
    errorbar_data <- loadings
    if (ci_bounds_effective == "arrow") {
      errorbar_data$ci.lower.plot <- pmax(errorbar_data$ci.lower, -1)
      errorbar_data$ci.upper.plot <- pmin(errorbar_data$ci.upper, 1)
    } else {
      errorbar_data$ci.lower.plot <- errorbar_data$ci.lower
      errorbar_data$ci.upper.plot <- errorbar_data$ci.upper
    }
    plot <- plot + ggplot2::geom_errorbar(
      data = errorbar_data,
      ggplot2::aes(xmin = .data$ci.lower.plot, xmax = .data$ci.upper.plot),
      width = 0.2, color = "grey50", orientation = "y"
    )
    if (ci_bounds_effective == "arrow") {
      arrow_span <- 0.05
      arrow_left <- errorbar_data[!is.na(errorbar_data$ci.lower) & errorbar_data$ci.lower < -1, , drop = FALSE]
      arrow_right <- errorbar_data[!is.na(errorbar_data$ci.upper) & errorbar_data$ci.upper > 1, , drop = FALSE]
      if (nrow(arrow_left) > 0) {
        arrow_left$x <- -1 + arrow_span
        arrow_left$xend <- -1
      }
      if (nrow(arrow_right) > 0) {
        arrow_right$x <- 1 - arrow_span
        arrow_right$xend <- 1
      }
      arrow_data <- rbind(arrow_left, arrow_right)
      if (nrow(arrow_data) > 0) {
        plot <- plot + ggplot2::geom_segment(
          data = arrow_data,
          ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$rhs, yend = .data$rhs),
          inherit.aes = FALSE,
          color = "grey50",
          linewidth = 0.4,
          arrow = grid::arrow(length = grid::unit(0.08, "inches"))
        )
      }
    }
  }

  plot
}
