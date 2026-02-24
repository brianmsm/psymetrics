#' Extract Parameter Estimates from a lavaan Model
#'
#' @description
#' `model_estimates.lavaan()` extracts CFA/SEM parameter
#' estimates from a fitted `lavaan` object and organizes
#' them into components (for example, loadings, regressions,
#' variances, and thresholds). The output is designed to
#' integrate with `psymetrics` formatting and export methods.
#'
#' @param fit A fitted `lavaan` object (for example from
#'   `lavaan::cfa()`, `lavaan::sem()`, or `lavaan::growth()`).
#' @param standardized Controls standardized extraction.
#'   Accepted values are `FALSE`, `TRUE`, `"std.all"`,
#'   `"std.lv"`, `"std.nox"`, plus aliases:
#'   `"all"`, `"latent"`, `"lv"`, and `"no_exogenous"`.
#' @param ci Confidence level for interval columns. Must be a
#'   numeric value in `(0, 1)`.
#' @param component Character vector indicating which parameter
#'   components to keep. Use `"all"` (default) to keep all
#'   available components.
#' @param verbose Logical; if `TRUE`, prints informative messages
#'   about non-convergence and partially unavailable inferential
#'   statistics.
#' @param ... Additional arguments passed to methods.
#'
#' @return A data frame with class `model_estimates`.
#'   Core columns are:
#'   `To`, `Operator`, `From`, `Coefficient`, `SE`, `CI_low`,
#'   `CI_high`, `z`, `p`, `Component`, and `converged`.
#'
#' @seealso [model_estimates()] for the generic.
#' @export
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'
#'   sem_model <- '
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'   '
#'
#'   fit <- sem(sem_model, data = PoliticalDemocracy)
#'   model_estimates(fit)
#'   model_estimates(fit, component = c("loading", "regression"))
#'   model_estimates(fit, standardized = "std.lv")
#' } else {
#'   message("Please install 'lavaan' to run this example.")
#' }
model_estimates.lavaan <- function(fit,
                                   standardized = FALSE,
                                   ci = 0.95,
                                   component = "all",
                                   verbose = TRUE,
                                   ...) {
  rlang::check_installed("lavaan", reason = "to process 'lavaan' objects.")

  if (!is.logical(verbose) || length(verbose) != 1L) {
    cli::cli_abort("`verbose` must be a single logical value.")
  }

  ci <- lavaan_estimates_normalize_ci(ci, verbose = verbose)
  standardized_type <- lavaan_estimates_normalize_standardized(standardized)
  component_filter <- lavaan_estimates_normalize_component(component)
  converged <- isTRUE(lavaan::lavInspect(fit, "converged"))

  if (!converged) {
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(
        "The model did not converge. Returning available parameter estimates; some inferential columns may be NA."
      )
    }
  }

  estimates_raw <- if (standardized_type == "none") {
    lavaan::parameterEstimates(
      fit,
      ci = TRUE,
      level = ci,
      standardized = FALSE,
      remove.eq = FALSE,
      remove.ineq = FALSE,
      remove.system.eq = FALSE
    )
  } else {
    lavaan::standardizedSolution(
      fit,
      type = standardized_type,
      ci = TRUE,
      level = ci,
      remove.eq = FALSE,
      remove.ineq = FALSE,
      remove.def = FALSE
    )
  }

  coefficient_col <- if (standardized_type == "none") "est" else "est.std"
  estimates <- lavaan_build_estimates_table(
    estimates_raw,
    coefficient_col = coefficient_col,
    verbose = verbose,
    converged = converged
  )

  if (!all(component_filter == "all")) {
    available_components <- unique(estimates$Component)
    missing_components <- setdiff(component_filter, available_components)

    estimates <- estimates[estimates$Component %in% component_filter, , drop = FALSE]
    rownames(estimates) <- NULL

    if (isTRUE(verbose) && length(missing_components) > 0L) {
      cli::cli_inform(
        "Requested component{?s} not present in this model: {missing_components}."
      )
    }

    if (nrow(estimates) == 0L && isTRUE(verbose)) {
      cli::cli_inform(
        "No parameters matched `component = {component}`."
      )
    }
  }

  class(estimates) <- c("model_estimates", class(estimates))
  attr(estimates, "ci") <- ci
  attr(estimates, "standardized") <- standardized_type
  estimates
}


# Helpers -----------------------------------------------------------------

lavaan_build_estimates_table <- function(estimates_raw, coefficient_col, verbose = TRUE,
                                         converged = TRUE) {
  required_cols <- c("lhs", "op", "rhs")
  missing_required <- setdiff(required_cols, names(estimates_raw))
  if (length(missing_required) > 0L) {
    cli::cli_abort(
      "Could not extract required `lavaan` columns: {.field {missing_required}}."
    )
  }

  column_map <- c(
    Coefficient = coefficient_col,
    SE = "se",
    CI_low = "ci.lower",
    CI_high = "ci.upper",
    z = "z",
    p = "pvalue"
  )

  out <- data.frame(
    To = as.character(estimates_raw$lhs),
    Operator = as.character(estimates_raw$op),
    From = as.character(estimates_raw$rhs),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (target_col in names(column_map)) {
    source_col <- column_map[[target_col]]
    if (source_col %in% names(estimates_raw)) {
      out[[target_col]] <- suppressWarnings(as.numeric(estimates_raw[[source_col]]))
    } else {
      out[[target_col]] <- NA_real_
    }
  }

  optional_map <- c(Group = "group", Level = "level", Label = "label")
  for (target_col in names(optional_map)) {
    source_col <- optional_map[[target_col]]
    if (source_col %in% names(estimates_raw)) {
      out[[target_col]] <- estimates_raw[[source_col]]
    }
  }

  out$Component <- lavaan_map_estimate_component(out$Operator, out$To, out$From)
  out$converged <- isTRUE(converged)

  missing_source_cols <- unname(column_map)[!unname(column_map) %in% names(estimates_raw)]
  missing_report <- character(0)
  if (isTRUE(verbose) && length(missing_source_cols) > 0L) {
    missing_report <- names(column_map)[match(missing_source_cols, unname(column_map))]
    cli::cli_inform(
      "Some inferential columns are unavailable and were set to NA: {missing_report}."
    )
  }

  inferential_cols <- intersect(c("SE", "CI_low", "CI_high", "z", "p"), names(out))
  inferential_na <- inferential_cols[vapply(out[inferential_cols], function(col) {
    all(is.na(col))
  }, logical(1))]
  inferential_na <- setdiff(inferential_na, missing_report)
  if (isTRUE(verbose) && length(inferential_na) > 0L) {
    cli::cli_inform(
      "Some inferential columns are fully unavailable in this fit and were kept as NA: {inferential_na}."
    )
  }

  negative_variance <- out$Component == "Variance" &
    !is.na(out$Coefficient) &
    out$Coefficient < 0
  if (isTRUE(verbose) && any(negative_variance)) {
    n_negative <- sum(negative_variance)
    cli::cli_alert_warning(
      "Potential Heywood case detected: {n_negative} variance parameter{?s} below zero."
    )
  }

  out
}

lavaan_map_estimate_component <- function(operator, to, from) {
  operator <- as.character(operator)
  to <- as.character(to)
  from <- as.character(from)

  component <- rep("Other", length(operator))
  component[operator == "=~"] <- "Loading"
  component[operator == "~1"] <- "Mean"

  regression_idx <- operator == "~" & !is.na(from) & from != "1"
  component[regression_idx] <- "Regression"

  variance_idx <- operator == "~~" & !is.na(to) & !is.na(from) & to == from
  correlation_idx <- operator == "~~" & !is.na(to) & !is.na(from) & to != from
  component[variance_idx] <- "Variance"
  component[correlation_idx] <- "Correlation"

  component[operator == ":="] <- "Defined"
  component[operator == "|"] <- "Threshold"
  component
}

lavaan_estimates_normalize_standardized <- function(standardized) {
  if (is.logical(standardized) && length(standardized) == 1L) {
    return(if (isTRUE(standardized)) "std.all" else "none")
  }

  if (!is.character(standardized) || length(standardized) != 1L) {
    cli::cli_abort(
      "`standardized` must be one of FALSE, TRUE, \"std.all\", \"std.lv\", \"std.nox\", \"all\", \"latent\", \"lv\", or \"no_exogenous\"."
    )
  }

  standardized_key <- tolower(trimws(standardized))
  standardized_map <- c(
    "false" = "none",
    "none" = "none",
    "raw" = "none",
    "unstandardized" = "none",
    "unstd" = "none",
    "true" = "std.all",
    "all" = "std.all",
    "std.all" = "std.all",
    "latent" = "std.lv",
    "lv" = "std.lv",
    "std.lv" = "std.lv",
    "no_exogenous" = "std.nox",
    "noexogenous" = "std.nox",
    "nox" = "std.nox",
    "std.nox" = "std.nox"
  )

  if (!standardized_key %in% names(standardized_map)) {
    cli::cli_abort(
      "`standardized` must be one of FALSE, TRUE, \"std.all\", \"std.lv\", \"std.nox\", \"all\", \"latent\", \"lv\", or \"no_exogenous\"."
    )
  }

  unname(standardized_map[[standardized_key]])
}

lavaan_estimates_normalize_ci <- function(ci, verbose = TRUE) {
  if (!is.numeric(ci) || length(ci) == 0L || anyNA(ci)) {
    cli::cli_abort("`ci` must be a numeric value in (0, 1).")
  }

  if (length(ci) > 1L && isTRUE(verbose)) {
    cli::cli_inform(
      "Multiple values were supplied to `ci`; using the first value ({ci[1]})."
    )
  }

  ci <- ci[1]
  if (ci <= 0 || ci >= 1) {
    cli::cli_abort("`ci` must be a numeric value in (0, 1).")
  }

  ci
}

lavaan_estimates_normalize_component <- function(component) {
  if (is.null(component)) {
    return("all")
  }
  if (!is.character(component) || length(component) == 0L) {
    cli::cli_abort(
      "`component` must be a character vector with values such as \"all\", \"loading\", \"regression\", \"correlation\", \"variance\", \"mean\", \"defined\", \"threshold\", or \"other\"."
    )
  }

  component_key <- tolower(trimws(component))
  component_map <- c(
    "all" = "all",
    "loading" = "Loading",
    "loadings" = "Loading",
    "regression" = "Regression",
    "regressions" = "Regression",
    "correlation" = "Correlation",
    "correlations" = "Correlation",
    "variance" = "Variance",
    "variances" = "Variance",
    "mean" = "Mean",
    "means" = "Mean",
    "intercept" = "Mean",
    "intercepts" = "Mean",
    "defined" = "Defined",
    "threshold" = "Threshold",
    "thresholds" = "Threshold",
    "other" = "Other"
  )

  unknown_components <- unique(component_key[!component_key %in% names(component_map)])
  if (length(unknown_components) > 0L) {
    cli::cli_abort(c(
      "Unsupported values in `component`: {.field {unknown_components}}.",
      "Allowed values: all, loading, regression, correlation, variance, mean, defined, threshold, other."
    ))
  }

  normalized <- unname(component_map[component_key])
  if ("all" %in% normalized) {
    return("all")
  }

  unique(normalized)
}
