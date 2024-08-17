#' Extract Fit Indices from a lavaan Model
#'
#' @description `model_fit.lavaan` extracts fit indices from a `lavaan` model object.
#' The function allows you to specify the type of indices to extract: `"standard"`,
#' `"scaled"`, or `"robust"`. If the model uses a robust estimator and you specify
#' `type = "scaled"` or `type = "robust"`, the corresponding indices will be returned.
#' If no type is specified, the function automatically chooses `"scaled"` for robust estimators
#' and `"standard"` otherwise.
#'
#' @param x A `lavaan` object estimated with `lavaan::cfa()`, `lavaan::sem()`, or similar functions.
#' @param type A character string specifying the type of fit indices to extract.
#'   Options are `"standard"`, `"scaled"`, and `"robust"`. Defaults to `NULL`,
#'   which will automatically choose `"scaled"` if a robust estimator is used; otherwise `"standard"`.
#' @param metrics A character vector specifying the fit indices to return. The default is `"essential"`,
#'   which includes common fit indices. You can also specify a custom set of metrics.
#' @param verbose A logical value indicating whether to display informational messages about
#'   metric adjustments. Defaults to `TRUE`.
#' @return A data frame containing the specified fit indices of the model.
#' @export
#' @examples
#' library(lavaan)
#' model <- 'visual  =~ x1 + x2 + x3
#'           textual =~ x4 + x5 + x6
#'           speed   =~ x7 + x8 + x9'
#' fit <- cfa(model, data = HolzingerSwineford1939,
#'            estimator = "MLR")
#' model_fit(fit)
#' model_fit(fit, type = "robust")
#' model_fit(fit, metrics = c("cfi", "tli"))

model_fit.lavaan <- function(x, type = NULL, metrics = "essential", verbose = TRUE) {
  # Determine if a robust estimator is being used
  robust_type <- is_robust_estimator_lavaan(x)

  # Determine the type of index to use
  if (is.null(type)) {
    if (robust_type == "robust") {
      type <- "scaled"
    } else {
      type <- "standard"
    }
  }

  # Validate 'type'
  if (type %in% c("scaled", "robust") && robust_type != "robust") {
    cli::cli_alert_danger(
      paste0(
        "The model was not estimated with a robust estimator. ",
        "'scaled' and 'robust' indices are not available."
      )
    )
    return(data.frame())
  }

  if (type == "standard" && robust_type == "robust") {
    cli::cli_alert_warning(
      paste0(
        "You are using a robust estimator but requesting 'standard' indices. ",
        "It is recommended to use 'scaled' or 'robust' indices that correspond ",
        "to the estimator used."
      )
    )
  }

  # Check if the model converged
  if (!lavaan::lavInspect(x, "converged")) {
    cli::cli_alert_danger(
      paste0(
        "The model did not converge. ",
        "Fit indices are not available."
      )
    )
    return(data.frame())
  }

  # Extract fit indices based on the type and metrics
  fit_measure <- extract_fit_lavaan(x, type, metrics = metrics, verbose = verbose)

  return(fit_measure)
}

# Internal function to extract fit indices based on type and metrics
extract_fit_lavaan <- function(x, type, metrics, verbose) {

  original_metrics <- metrics

  # Define essential (classical) metrics if metrics is "essential"
  if (length(metrics) == 1 && metrics == "essential") {
    metrics <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
  } else {
    metrics <- tolower(metrics)
  }

  # Lists of available scaled and robust metrics
  scaled_metrics <- c("chisq", "df", "pvalue", "baseline.chisq", "baseline.df", "baseline.pvalue",
                      "cfi", "tli", "nnfi", "rfi", "nfi", "pnfi", "ifi", "rni",
                      "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "rmsea.notclose.pvalue")

  robust_metrics <- c("cfi", "tli", "nnfi", "rni", "rmsea",
                      "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "rmsea.notclose.pvalue")

  # Initialize a vector to track which metrics have been automatically adjusted
  adjusted_metrics <- c()

  # Adjust metrics based on the selected type
  metrics_to_use <- sapply(metrics, function(metric) {
    if (grepl("\\.(scaled|robust)$", metric)) {
      return(metric)  # Return as is if already specified
    } else if (type == "scaled" && metric %in% scaled_metrics) {
      adjusted_metrics <<- c(adjusted_metrics, metric)
      return(paste0(metric, ".scaled"))
    } else if (type == "robust") {
      if (metric %in% robust_metrics) {
        adjusted_metrics <<- c(adjusted_metrics, metric)
        return(paste0(metric, ".robust"))
      } else if (metric %in% scaled_metrics) {
        adjusted_metrics <<- c(adjusted_metrics, metric)
        return(paste0(metric, ".scaled"))
      } else {
        return(metric)
      }
    } else {
      return(metric)
    }
  })

  # Display an informational message if any metrics were adjusted and verbose is TRUE
  if (verbose && length(adjusted_metrics) > 0 && length(original_metrics) != 1) {
    cli::cli_inform(c(
      cli::cli_text("{.field {adjusted_metrics}} were adjusted to their {type} version{?s}."),
      "If you want to control the specific metric type used, specify it explicitly (e.g., {.code cfi.robust}) or modify the {.field type} argument."
    ))
  }

  # Extract the metrics based on metrics_to_use
  fit_measures <- lavaan::fitmeasures(x, fit.measures = c("npar", metrics_to_use))

  # Transpose and convert to a data frame, removing the special class
  fit_measure_df <- as.data.frame(unclass(t(fit_measures)), stringsAsFactors = FALSE)

  # Rename the columns to be more descriptive
  colnames(fit_measure_df) <- c("NPAR", toupper(metrics))

  # Add additional columns
  fit_measure_df <- data.frame(
    NOBS = sum(lavaan::lavInspect(x, "nobs")),
    ESTIMATOR = lavaan_estimator(x),
    NGROUPS = lavaan::lavInspect(x, "ngroups"),
    fit_measure_df,
    row.names = NULL  # Remove row names
  )

  # Clean up the column names to remove ".scaled" and ".robust"
  colnames(fit_measure_df) <- gsub("\\.(SCALED|ROBUST)$", "", colnames(fit_measure_df))

  # Add a "converged" column only if the model did not converge
  if (!lavaan::lavInspect(x, "converged")) {
    fit_measure_df$converged <- FALSE
  }

  return(fit_measure_df)
}


# Helper functions ------------------------------------------------------------

lavaan_estimator <- function(x) {
  if (lavaan::lavInspect(x, "options")$estimator == "DWLS") {
    if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
        lavaan::lavInspect(x, "options")$test == "satorra.bentler") {
      estimator <- "WLSM"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
               lavaan::lavInspect(x, "options")$test == "mean.var.adjusted") {
      estimator <- "WLSMVS"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
               lavaan::lavInspect(x, "options")$test == "scaled.shifted") {
      estimator <- "WLSMV"
    } else if (lavaan::lavInspect(x, "options")$se == "standard" &
               lavaan::lavInspect(x, "options")$test == "standard") {
      estimator <- "DWLS"
    } else {
      estimator <- "DWLS_variant"
    }
  } else if (lavaan::lavInspect(x, "options")$estimator == "ULS") {
    if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
        lavaan::lavInspect(x, "options")$test == "satorra.bentler") {
      estimator <- "ULSM"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
               lavaan::lavInspect(x, "options")$test == "mean.var.adjusted") {
      estimator <- "ULSMVS"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
               lavaan::lavInspect(x, "options")$test == "scaled.shifted") {
      estimator <- "ULSMV"
    } else if (lavaan::lavInspect(x, "options")$se == "standard" &
               lavaan::lavInspect(x, "options")$test == "standard") {
      estimator <- "ULS"
    } else {
      estimator <- "ULS_variant"
    }
  } else if (lavaan::lavInspect(x, "options")$estimator == "ML") {
    if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
        lavaan::lavInspect(x, "options")$test == "satorra.bentler") {
      estimator <- "MLM"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.huber.white" &
               lavaan::lavInspect(x, "options")$test %in% c(
                 "yuan.bentler.mplus",
                 "yuan.bentler"
               )) {
      estimator <- "MLR"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
               lavaan::lavInspect(x, "options")$test == "mean.var.adjusted") {
      estimator <- "MLMVS"
    } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
               lavaan::lavInspect(x, "options")$test == "scaled.shifted") {
      estimator <- "MLMV"
    } else if (lavaan::lavInspect(x, "options")$se == "standard" &
               lavaan::lavInspect(x, "options")$test == "standard" &
               unique(lavaan::lavInspect(x, "options")$information) == "expected") {
      estimator <- "ML"
    } else if (lavaan::lavInspect(x, "options")$se == "standard" &
               lavaan::lavInspect(x, "options")$test == "standard" &
               unique(lavaan::lavInspect(x, "options")$information) == "first.order") {
      estimator <- "MLF"
    } else {
      estimator <- "ML_variant"
    }
  } else {
    estimator <- lavaan::lavInspect(x, "options")$estimator
  }

  return(estimator)
}

is_robust_estimator_lavaan <- function(x) {
  if (lavaan::lavInspect(x, "options")$test %in% c(
    "satorra.bentler",
    "yuan.bentler",
    "yuan.bentler.mplus",
    "mean.var.adjusted",
    "scaled.shifted"
  )) {
    type <- "robust"
  } else {
    type <- "non-robust"
  }
  return(type)
}
