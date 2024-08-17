#' Extract Fit Indices from a lavaan Model
#'
#' @description `model_fit.lavaan` extracts fit indices from a lavaan model.
#' You can specify the type of indices to extract: `"standard"`, `"scaled"`,
#' or `"robust"`. If the model uses a robust estimator and you specify `type = "scaled"`
#' or `type = "robust"`, the corresponding indices will be returned.
#'
#' @param x A `lavaan` object estimated with `lavaan::cfa()` or `lavaan::sem()`.
#' @param type A character string specifying the type of fit indices to extract.
#'   Options are `"standard"`, `"scaled"`, and `"robust"`. Defaults to `NULL`,
#'   which will choose `"scaled"` if a robust estimator is used; otherwise `"standard"`.
#' @return A data frame containing the fit indices of the model.
#' @export
#' @examples
#' library(lavaan)
#' model <- 'visual  =~ x1 + x2 + x3'
#' fit <- cfa(model, data = HolzingerSwineford1939)
#' model_fit(fit)

model_fit.lavaan <- function(x, type = NULL, metrics = "essential") {
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
  fit_measure <- extract_fit_lavaan(x, type, metrics = metrics)

  return(fit_measure)
}

# Internal function to extract fit indices based on type and metrics
extract_fit_lavaan <- function(x, type, metrics = "essential") {
  # Define essential (classical) metrics if metrics is "essential"
  if (length(metrics) == 1 && metrics == "essential") {
    metrics <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
  } else{
    metrics <- tolower(metrics)
  }

  # Lists of available scaled and robust metrics
  scaled_metrics <- c("chisq", "df", "pvalue", "baseline.chisq", "baseline.df", "baseline.pvalue",
                      "cfi", "tli", "nnfi", "rfi", "nfi", "pnfi", "ifi", "rni",
                      "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "rmsea.notclose.pvalue")

  robust_metrics <- c("cfi", "tli", "nnfi", "rni", "rmsea",
                      "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "rmsea.notclose.pvalue")

  # Adjust metrics based on the selected type
  metrics_to_use <- sapply(metrics, function(metric) {
    if (type == "scaled") {
      if (metric %in% scaled_metrics) {
        return(paste0(metric, ".scaled"))
      } else {
        return(metric)
      }
    } else if (type == "robust") {
      if (metric %in% robust_metrics) {
        return(paste0(metric, ".robust"))
      } else if (metric %in% scaled_metrics) {
        return(paste0(metric, ".scaled"))
      } else {
        return(metric)
      }
    } else {
      return(metric)
    }
  })

  # Extract the metrics based on metrics_to_use
  fit_measures <- lavaan::fitmeasures(x, fit.measures = c("npar", metrics_to_use))

  # Transpose and convert to a data frame, removing the special class
  fit_measure_df <- as.data.frame(unclass(t(fit_measures)), stringsAsFactors = FALSE)

  # Rename the columns to be more descriptive
  colnames(fit_measure_df) <- c("npar", toupper(metrics))

  # Add additional columns
  fit_measure_df <- data.frame(
    nobs = sum(lavaan::lavInspect(x, "nobs")),
    estimator = lavaan_estimator(x),
    ngroups = lavaan::lavInspect(x, "ngroups"),
    fit_measure_df,
    row.names = NULL # Remove row names
  )

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
