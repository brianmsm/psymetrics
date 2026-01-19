#' Extract Fit Indices from a lavaan Model
#'
#' @description
#' `model_fit.lavaan` extracts fit indices from a `lavaan`
#' model object. The function allows you to specify the
#' type of indices to extract: `"standard"`, `"scaled"`,
#' or `"robust"`. If the model uses a robust estimator
#' and you specify `type = "scaled"` or `type = "robust"`,
#' the corresponding indices will be returned. If no
#' type is specified, the function automatically chooses
#' `"scaled"` for robust estimators and `"standard"` otherwise.
#' When the model was fitted with multiple tests, the function
#' can return multiple rows (one per non-standard test).
#'
#' @param fit A `lavaan` object estimated with `lavaan::cfa()`,
#'   `lavaan::sem()`, or similar functions.
#' @param type A character string specifying the type of
#'   fit indices to extract. Options are `"standard"`,
#'   `"scaled"`, and `"robust"`. Defaults to `NULL`,
#'   which will automatically choose `"scaled"` if a
#'   robust estimator is used; otherwise `"standard"`.
#' @param metrics A character vector specifying the fit
#'   indices to return. The default is `"essential"`,
#'   which includes common fit indices. You can also
#'   specify a custom set of metrics.
#' @param verbose A logical value indicating whether to
#'   display informational messages about metric adjustments.
#'   Defaults to `TRUE`.
#' @param test A character string or vector specifying which
#'   lavaan tests to report. Use `"default"` (the default) to
#'   return all non-standard tests from
#'   `lavInspect(fit, "options")$test`, excluding `"standard"`,
#'   `"default"`, and `"none"`. Provide a character vector to
#'   request specific tests; unavailable entries are dropped
#'   with an informational message when `verbose = TRUE`.
#' @param standard_test A logical value indicating whether to
#'   include a standard-test row in addition to non-standard
#'   tests. When `TRUE`, the standard row is shown first and
#'   always uses standard indices.
#' @param test_details A logical value indicating whether to
#'   add `TEST` and `SE` columns describing the test and standard
#'   error settings used to compute the fit measures. When `FALSE`
#'   (default), `ESTIMATOR` substitutes the test name for unknown
#'   estimator variants.
#' @param ... Additional arguments passed to methods.
#'
#' @return A data frame containing the specified fit
#'   indices of the model. When multiple tests are reported,
#'   the data frame can include multiple rows.
#' @seealso [model_fit] for an overview of model fit
#'   methods in the package.
#' @export
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'   hs_model <- 'visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9'
#'   fit <- cfa(hs_model, data = HolzingerSwineford1939,
#'              estimator = "MLR")
#'   model_fit(fit)
#'   model_fit(fit, type = "robust")
#'   model_fit(fit, metrics = c("cfi", "tli"))
#' } else {
#'   message("Please install 'lavaan' to run this example.")
#' }
model_fit.lavaan <- function(fit, type = NULL, metrics = "essential", verbose = TRUE,
                             test = "default", standard_test = FALSE, test_details = FALSE, ...) {
  rlang::check_installed("lavaan", reason = "to process 'lavaan' objects.")
  dots <- list(...)
  standard_test_message <- TRUE
  if ("standard_test_message" %in% names(dots)) {
    standard_test_message <- dots$standard_test_message
    if (!is.logical(standard_test_message) || length(standard_test_message) != 1L) {
      rlang::abort("`standard_test_message` must be TRUE or FALSE.")
    }
  }
  # Determine if a robust estimator is being used
  robust_type <- is_robust_estimator_lavaan(fit)
  lav_options <- lavaan::lavInspect(fit, "options")
  test_groups <- lavaan_test_groups()
  test_vec <- lav_options$test
  has_browne <- lavaan_has_test(test_vec, test_groups$browne_tests)
  has_robust <- lavaan_has_test(test_vec, test_groups$robust_tests)
  has_bootstrap <- lavaan_has_test(test_vec, test_groups$bootstrap_tests)
  has_none <- lavaan_has_test(test_vec, "none")
  browne_only <- has_browne && !has_robust

  if (is.null(test)) {
    test <- "default"
  }
  if (!is.character(test) || length(test) == 0L) {
    rlang::abort("`test` must be a non-empty character vector or \"default\".")
  }
  if (!is.logical(standard_test) || length(standard_test) != 1L) {
    rlang::abort("`standard_test` must be a single logical value.")
  }
  if (!is.logical(test_details) || length(test_details) != 1L) {
    rlang::abort("`test_details` must be a single logical value.")
  }

  # Determine the type of index to use
  if (is.null(type)) {
    if (robust_type == "robust") {
      type <- "scaled"
    } else {
      type <- "standard"
    }
  }

  exclude_tests <- c("standard", "default", "none")
  available_tests <- if (is.null(test_vec)) {
    character(0)
  } else {
    test_vec
  }

  if (length(test) == 1L && test == "default") {
    selected_tests <- available_tests[!available_tests %in% exclude_tests]
  } else {
    requested_tests <- test
    requested_tests <- requested_tests[!requested_tests %in% exclude_tests]
    missing_tests <- setdiff(requested_tests, available_tests)
    if (length(missing_tests) > 0L && verbose) {
      cli::cli_inform(
        cli::cli_text(
          "Requested tests not found in the fitted model and were dropped: {missing_tests}."
        )
      )
    }
    selected_tests <- requested_tests[requested_tests %in% available_tests]
  }
  selected_tests <- unique(selected_tests)
  available_nonstandard <- available_tests[!available_tests %in% exclude_tests]

  if (length(selected_tests) == 0L && type %in% c("scaled", "robust")) {
    if (verbose && !has_none) {
      if (browne_only) {
        cli::cli_inform(
          "Browne residual tests do not provide scaled or robust fit measures; using standard indices instead."
        )
      } else if (has_bootstrap) {
        cli::cli_inform(
          "Bollen-Stine bootstrap tests do not provide scaled or robust fit measures; using standard indices instead."
        )
      } else if (length(available_nonstandard) == 0L) {
        cli::cli_inform(
          "The model reports only standard tests; using standard indices instead."
        )
      } else {
        cli::cli_inform(
          "No requested non-standard tests are available; using standard indices instead."
        )
      }
    }
    type <- "standard"
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
  if (!lavaan::lavInspect(fit, "converged")) {
    cli::cli_alert_danger(
      paste0(
        "The model did not converge. ",
        "Fit indices are not available."
      )
    )
    return(data.frame())
  }

  standard_estimator <- lavaan_standard_estimator(fit)
  if (length(selected_tests) == 0L) {
    fit_measure <- extract_fit_lavaan(
      fit,
      type,
      metrics = metrics,
      verbose = verbose,
      estimator_override = standard_estimator,
      test_details = test_details
    )
    return(fit_measure)
  }

  if (isTRUE(standard_test) &&
      type %in% c("scaled", "robust") &&
      isTRUE(standard_test_message)) {
    cli::cli_inform(
      cli::cli_text(
        "Standard-test row uses standard indices for estimator {standard_estimator}."
      )
    )
  }

  fit_measure_list <- list()
  if (isTRUE(standard_test)) {
    fit_measure_list[[length(fit_measure_list) + 1L]] <- extract_fit_lavaan(
      fit,
      type = "standard",
      metrics = metrics,
      verbose = verbose,
      estimator_override = standard_estimator,
      test_details = test_details
    )
  }

  for (selected_test in selected_tests) {
    fit_measure_list[[length(fit_measure_list) + 1L]] <- extract_fit_lavaan(
      fit,
      type = type,
      metrics = metrics,
      verbose = verbose,
      scaled_test = selected_test,
      test_details = test_details
    )
  }

  fit_measure <- do.call(rbind, fit_measure_list)
  class(fit_measure) <- unique(c("model_fit", class(fit_measure)))
  return(fit_measure)
}

# Internal function to extract fit indices based on type and metrics
extract_fit_lavaan <- function(fit, type, metrics, verbose,
                               scaled_test = NULL, estimator_override = NULL,
                               test_details = FALSE) {

  original_metrics <- metrics
  lav_options <- lavaan::lavInspect(fit, "options")
  test_groups <- lavaan_test_groups()
  test_vec <- lav_options$test
  if (!is.null(scaled_test)) {
    test_vec <- c("standard", scaled_test)
  }
  has_robust <- lavaan_has_test(test_vec, test_groups$robust_tests)
  has_browne <- lavaan_has_test(test_vec, test_groups$browne_tests)
  has_bootstrap <- lavaan_has_test(test_vec, test_groups$bootstrap_tests)
  has_standard_only <- lavaan_has_test(test_vec, test_groups$standard_only_tests)
  has_none <- lavaan_has_test(test_vec, "none")
  browne_only <- has_browne && !has_robust
  standard_only <- browne_only || has_bootstrap || has_standard_only
  standard_test <- lav_options$standard.test
  if (is.null(standard_test)) {
    standard_test <- "standard"
  }

  test_label <- if (!is.null(scaled_test)) {
    scaled_test
  } else if (has_none) {
    "none"
  } else {
    standard_test
  }
  test_label <- test_label[1]
  se_label <- lav_options$se
  if (is.null(se_label) || length(se_label) == 0L) {
    se_label <- "standard"
  } else {
    se_label <- se_label[1]
  }

  estimator_label <- estimator_override
  estimator_warning <- estimator_override
  if (is.null(estimator_label)) {
    estimator_label <- lavaan_estimator(fit, test_vec = test_vec)
    estimator_warning <- estimator_label
  }
  if (is.null(estimator_warning)) {
    estimator_warning <- estimator_label
  }
  if (is.null(estimator_override) && !isTRUE(test_details) &&
      grepl("_variant$", estimator_label)) {
    estimator_label <- test_label
  }

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

  if (standard_only && type %in% c("scaled", "robust")) {
    if (verbose && !has_none) {
      if (browne_only) {
        cli::cli_inform(
          "Browne residual tests do not provide scaled or robust fit measures; using standard indices instead."
        )
      } else if (has_bootstrap) {
        cli::cli_inform(
          "Bollen-Stine bootstrap tests do not provide scaled or robust fit measures; using standard indices instead."
        )
      } else {
        cli::cli_inform(
          "The model does not report a chi-square test; using standard indices instead."
        )
      }
    }
    type <- "standard"
  }

  if (has_none) {
    if (verbose) {
      cli::cli_alert_warning(
        "Fit measures are not available when test = 'none'; returning NA for requested metrics."
      )
    }
    if (isTRUE(test_details)) {
      fit_measure_df <- data.frame(
        NOBS = sum(lavaan::lavInspect(fit, "nobs")),
        ESTIMATOR = estimator_label,
        TEST = test_label,
        SE = se_label,
        NPAR = lavaan::lavInspect(fit, "npar"),
        matrix(NA_real_, nrow = 1, ncol = length(metrics)),
        row.names = NULL,
        check.names = FALSE
      )
    } else {
      fit_measure_df <- data.frame(
        NOBS = sum(lavaan::lavInspect(fit, "nobs")),
        ESTIMATOR = estimator_label,
        NPAR = lavaan::lavInspect(fit, "npar"),
        matrix(NA_real_, nrow = 1, ncol = length(metrics)),
        row.names = NULL,
        check.names = FALSE
      )
    }
    base_cols <- c("NOBS", "ESTIMATOR")
    if (isTRUE(test_details)) {
      base_cols <- c(base_cols, "TEST", "SE")
    }
    base_cols <- c(base_cols, "NPAR")
    colnames(fit_measure_df) <- c(base_cols, toupper(metrics))

    colnames(fit_measure_df) <- gsub("\\.(SCALED|ROBUST)$", "", colnames(fit_measure_df))
    colnames(fit_measure_df) <- gsub("^RMSEA\\.CI\\.LOWER$", "RMSEA_CI_low", colnames(fit_measure_df))
    colnames(fit_measure_df) <- gsub("^RMSEA\\.CI\\.UPPER$", "RMSEA_CI_high", colnames(fit_measure_df))
    colnames(fit_measure_df) <- gsub("^CHISQ$", "Chi2", colnames(fit_measure_df))
    colnames(fit_measure_df) <- gsub("^DF$", "Chi2_df", colnames(fit_measure_df))
    colnames(fit_measure_df) <- gsub("^PVALUE$", "p_Chi2", colnames(fit_measure_df))

    class(fit_measure_df) <- c("model_fit", class(fit_measure_df))
    return(fit_measure_df)
  }

  if (verbose &&
      lav_options$estimator %in% c("ULS", "DWLS") &&
      lav_options$se == "robust.sem.nt" &&
      standard_test == "browne.residual.nt") {
    cli::cli_inform(cli::cli_text(
      "Since lavaan 0.6.21, {lav_options$estimator} with continuous data uses Browne's residual-based (NT) test by default; fit indices reflect that test."
    ))
  }

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
  if (is.null(scaled_test)) {
    fit_measures <- lavaan::fitmeasures(fit, fit.measures = c("npar", metrics_to_use))
  } else {
    fit_measures <- lavaan::fitmeasures(
      fit,
      fit.measures = c("npar", metrics_to_use),
      fm.args = list(scaled.test = scaled_test)
    )
  }

  expected_names <- c("npar", metrics_to_use)
  robust_targets <- metrics_to_use[grepl("\\.robust$", metrics_to_use)]
  if (type == "robust" && length(robust_targets) > 0L) {
    missing_robust <- setdiff(robust_targets, names(fit_measures))
    if (length(missing_robust) > 0L && verbose) {
      cli::cli_alert_warning(
        cli::cli_text(
          "Robust fit measures are not available for estimator {estimator_warning}; returning NA for requested robust metrics."
        )
      )
    }
  }

  fit_measures_full <- setNames(rep(NA_real_, length(expected_names)), expected_names)
  available_measures <- intersect(names(fit_measures), expected_names)
  fit_measures_full[available_measures] <- fit_measures[available_measures]
  fit_measures <- fit_measures_full

  # Transpose and convert to a data frame, removing the special class
  fit_measure_df <- as.data.frame(unclass(t(fit_measures)), stringsAsFactors = FALSE)

  # Rename the columns to be more descriptive
  colnames(fit_measure_df) <- c("NPAR", toupper(metrics))

  # Add additional columns
  if (isTRUE(test_details)) {
    fit_measure_df <- data.frame(
      NOBS = sum(lavaan::lavInspect(fit, "nobs")),
      ESTIMATOR = estimator_label,
      TEST = test_label,
      SE = se_label,
      fit_measure_df,
      row.names = NULL,  # Remove row names
      check.names = FALSE
    )
  } else {
    fit_measure_df <- data.frame(
      NOBS = sum(lavaan::lavInspect(fit, "nobs")),
      ESTIMATOR = estimator_label,
      fit_measure_df,
      row.names = NULL,  # Remove row names
      check.names = FALSE
    )
  }

  # Clean up the column names to remove ".scaled" and ".robust"
  colnames(fit_measure_df) <- gsub("\\.(SCALED|ROBUST)$", "", colnames(fit_measure_df))

  # Further clean-up of column names
  colnames(fit_measure_df) <- gsub("^RMSEA\\.CI\\.LOWER$", "RMSEA_CI_low", colnames(fit_measure_df))
  colnames(fit_measure_df) <- gsub("^RMSEA\\.CI\\.UPPER$", "RMSEA_CI_high", colnames(fit_measure_df))
  colnames(fit_measure_df) <- gsub("^CHISQ$", "Chi2", colnames(fit_measure_df))
  colnames(fit_measure_df) <- gsub("^DF$", "Chi2_df", colnames(fit_measure_df))
  colnames(fit_measure_df) <- gsub("^PVALUE$", "p_Chi2", colnames(fit_measure_df))

  # Add a "converged" column only if the model did not converge
  if (!lavaan::lavInspect(fit, "converged")) {
    fit_measure_df$converged <- FALSE
  }

  class(fit_measure_df) <- c("model_fit", class(fit_measure_df))

  return(fit_measure_df)
}


# Helper functions ------------------------------------------------------------

lavaan_test_groups <- function() {
  list(
    robust_tests = c(
      "satorra.bentler",
      "yuan.bentler",
      "yuan.bentler.mplus",
      "mean.var.adjusted",
      "scaled.shifted"
    ),
    browne_tests = c(
      "browne.residual.nt",
      "browne.residual.adf",
      "browne.residual.nt.model",
      "browne.residual.adf.model"
    ),
    bootstrap_tests = c("bollen.stine"),
    standard_only_tests = c("none", "default")
  )
}

lavaan_standard_estimator <- function(fit) {
  lav_options <- lavaan::lavInspect(fit, "options")
  estimator <- lav_options$estimator
  if (estimator == "ML") {
    info_type <- unique(lav_options$information)
    if (length(info_type) > 1L) {
      info_type <- info_type[1]
    }
    if (lav_options$se == "standard" && info_type == "first.order") {
      estimator <- "MLF"
    }
  }
  estimator
}

lavaan_has_test <- function(test_vec, candidates) {
  if (is.null(test_vec)) {
    return(FALSE)
  }
  any(test_vec %in% candidates)
}

lavaan_test_primary <- function(test_vec) {
  if (is.null(test_vec) || length(test_vec) == 0L) {
    return("standard")
  }

  test <- test_vec
  if (length(test) > 1L) {
    standard_idx <- which(test == "standard")
    if (length(standard_idx) > 0L) {
      test <- test[-standard_idx]
    }
    if (length(test) > 1L) {
      test <- test[1]
    }
  }

  if (length(test) == 0L) {
    "standard"
  } else {
    test[1]
  }
}

lavaan_estimator <- function(fit, test_vec = NULL) {
  lav_options <- lavaan::lavInspect(fit, "options")
  info_type <- unique(lav_options$information)
  if (length(info_type) > 1L) {
    info_type <- info_type[1]
  }
  test_groups <- lavaan_test_groups()
  if (is.null(test_vec)) {
    test_vec <- lav_options$test
  }
  primary_test <- lavaan_test_primary(test_vec)
  has_browne <- lavaan_has_test(test_vec, test_groups$browne_tests)
  has_bootstrap <- lavaan_has_test(test_vec, test_groups$bootstrap_tests)
  has_standard_only <- lavaan_has_test(test_vec, test_groups$standard_only_tests)
  standard_like <- primary_test == "standard" || has_browne || has_bootstrap || has_standard_only

  if (lav_options$estimator == "DWLS") {
    if (lav_options$se == "robust.sem" &&
        primary_test == "satorra.bentler") {
      estimator <- "WLSM"
    } else if (lav_options$se == "robust.sem" &&
               primary_test == "mean.var.adjusted") {
      estimator <- "WLSMVS"
    } else if (lav_options$se == "robust.sem" &&
               primary_test == "scaled.shifted") {
      estimator <- "WLSMV"
    } else if (lav_options$se == "robust.sem.nt" && has_browne) {
      estimator <- "DWLS"
    } else if (lav_options$se == "standard" && standard_like) {
      estimator <- "DWLS"
    } else {
      estimator <- "DWLS_variant"
    }
  } else if (lav_options$estimator == "ULS") {
    if (lav_options$se == "robust.sem" &&
        primary_test == "satorra.bentler") {
      estimator <- "ULSM"
    } else if (lav_options$se == "robust.sem" &&
               primary_test == "mean.var.adjusted") {
      estimator <- "ULSMVS"
    } else if (lav_options$se == "robust.sem" &&
               primary_test == "scaled.shifted") {
      estimator <- "ULSMV"
    } else if (lav_options$se == "robust.sem.nt" && has_browne) {
      estimator <- "ULS"
    } else if (lav_options$se == "standard" && standard_like) {
      estimator <- "ULS"
    } else {
      estimator <- "ULS_variant"
    }
  } else if (lav_options$estimator == "ML") {
    if (lav_options$se == "robust.sem" &&
        primary_test == "satorra.bentler") {
      estimator <- "MLM"
    } else if (lav_options$se == "robust.huber.white" &&
               (lavaan_has_test(test_vec, c("yuan.bentler.mplus", "yuan.bentler")) ||
                has_bootstrap || has_standard_only)) {
      estimator <- "MLR"
    } else if (lav_options$se == "robust.sem" &&
               primary_test == "mean.var.adjusted") {
      estimator <- "MLMVS"
    } else if (lav_options$se == "robust.sem" &&
               primary_test == "scaled.shifted") {
      estimator <- "MLMV"
    } else if (lav_options$se == "standard" &&
               standard_like &&
               info_type == "expected") {
      estimator <- "ML"
    } else if (lav_options$se == "standard" &&
               standard_like &&
               info_type == "first.order") {
      estimator <- "MLF"
    } else {
      estimator <- "ML_variant"
    }
  } else {
    estimator <- lav_options$estimator
  }

  return(estimator)
}

is_robust_estimator_lavaan <- function(fit) {
  lav_options <- lavaan::lavInspect(fit, "options")
  test_groups <- lavaan_test_groups()
  if (lavaan_has_test(lav_options$test, test_groups$robust_tests)) {
    type <- "robust"
  } else {
    type <- "non-robust"
  }
  return(type)
}
