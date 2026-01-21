#' Compare Model Fit Indices Across Multiple Models
#'
#' @description
#' `compare_model_fit()` compares the fit indices of two or more
#' models. It extracts the fit indices using [`model_fit()`]
#' and combines them into a single data frame for easy comparison.
#'
#' @param ... Two or more model objects to be compared.
#' @param type A character string specifying the type of fit
#'   indices to extract. Options are `"standard"`, `"scaled"`,
#'   and `"robust"`. Defaults to `NULL`, which automatically
#'   selects `"scaled"` if a robust estimator is used,
#'   otherwise `"standard"`.
#' @param metrics A character vector specifying which fit
#'   indices to extract. Defaults to `"essential"`, or a custom
#'   vector of indices.
#' @param verbose Logical. If `TRUE`, prints messages about
#'   the indices being adjusted.
#' @param test A character string or vector specifying which
#'   lavaan tests to report. Use `"default"` (the default) to
#'   return all non-standard tests from
#'   `lavInspect(fit, "options")$test`, excluding `"standard"`,
#'   `"default"`, and `"none"`. Provide a character vector to
#'   request specific tests; unavailable entries are dropped
#'   with an informational message when `verbose = TRUE`. When
#'   supplying a list, it must be named and each name must match
#'   the model labels shown in the `MODEL` column. If you supply
#'   named arguments, you may also reference the original object
#'   names when they are unambiguous.
#' @param standard_test A logical value indicating whether to
#'   include a standard-test row in addition to non-standard
#'   tests. When `TRUE`, the standard row is shown first and
#'   always uses standard indices. When supplying a list, it
#'   must be named and each name must match the model labels
#'   shown in the `MODEL` column. If you supply named arguments,
#'   you may also reference the original object names when they
#'   are unambiguous.
#' @param test_details Logical. If `TRUE`, include `TEST` and
#'   `SE` columns (for lavaan fits) that describe the test and
#'   standard error settings used to compute each row. When
#'   `standard_test = TRUE` adds the standard row, `SE` is set to
#'   `NA` for that row because fit indices do not depend on standard
#'   errors.
#'
#' @return A data frame containing the fit indices for each
#'   model, with an additional column identifying the models.
#' @seealso [model_fit] for an overview of model fit methods
#'   in the package.
#' @export
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'   hs_model <- 'visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9'
#'   fit1 <- cfa(hs_model, data = HolzingerSwineford1939, estimator = "ML")
#'   fit2 <- cfa(hs_model, data = HolzingerSwineford1939, estimator = "MLR")
#'   compare_model_fit(fit1, fit2)
#'   compare_model_fit(fit1, fit2, standard_test = TRUE, test_details = TRUE)
#'   compare_model_fit(fit1, fit2, metrics = c("cfi", "tli", "rmsea"))
#' } else {
#'   message("Please install 'lavaan' to run this example.")
#' }
compare_model_fit <- function(..., type = NULL, metrics = "essential", verbose = TRUE,
                              test = "default", standard_test = FALSE, test_details = FALSE) {
  # Capture all the fit objects as a list
  fits <- list(...)

  # Ensure at least two models are provided for comparison
  if (length(fits) < 2) {
    cli::cli_abort(
      c("At least two model fits must be provided for comparison")
    )
  }

  arg_names <- names(fits)
  deparsed_names <- sapply(substitute(list(...))[-1L], deparse)
  if (!is.null(arg_names)) {
    model_names <- ifelse(arg_names != "", arg_names, deparsed_names)
  } else {
    model_names <- deparsed_names
  }
  alias_names <- rep(NA_character_, length(model_names))
  if (!is.null(arg_names)) {
    alias_names[arg_names != ""] <- deparsed_names[arg_names != ""]
  }
  alias_names[alias_names == model_names] <- NA_character_
  alias_names[!is_simple_model_name(alias_names)] <- NA_character_
  default_test <- "default"
  default_standard_test <- FALSE
  if (!is.logical(test_details) || length(test_details) != 1L) {
    rlang::abort("`test_details` must be TRUE or FALSE.")
  }

  test_by_model <- resolve_test_by_model(test, model_names, alias_names, default_test)
  standard_test_by_model <- resolve_standard_test_by_model(
    standard_test,
    model_names,
    alias_names,
    default_standard_test
  )

  standard_estimators <- character(0)
  if (type %in% c("scaled", "robust") || is.null(type)) {
    for (fit_idx in seq_along(fits)) {
      fit <- fits[[fit_idx]]
      if (!inherits(fit, "lavaan")) {
        next
      }
      if (!isTRUE(standard_test_by_model[[fit_idx]])) {
        next
      }
      resolved_type <- type
      if (is.null(resolved_type)) {
        resolved_type <- if (is_robust_estimator_lavaan(fit) == "robust") {
          "scaled"
        } else {
          "standard"
        }
      }
      if (resolved_type %in% c("scaled", "robust")) {
        test_value <- test_by_model[[fit_idx]]
        if (is.null(test_value)) {
          test_value <- "default"
        }
        test_vec <- lavaan::lavInspect(fit, "options")$test
        if (is.null(test_vec)) {
          test_vec <- character(0)
        }
        exclude_tests <- c("standard", "default", "none")
        if (length(test_value) == 1L && test_value == "default") {
          selected_tests <- test_vec[!test_vec %in% exclude_tests]
        } else {
          requested_tests <- test_value
          requested_tests <- requested_tests[!requested_tests %in% exclude_tests]
          selected_tests <- requested_tests[requested_tests %in% test_vec]
        }
        if (length(selected_tests) == 0L) {
          resolved_type <- "standard"
        }
      }
      if (resolved_type %in% c("scaled", "robust")) {
        standard_estimators <- c(standard_estimators, lavaan_standard_estimator(fit))
      }
    }
  }
  standard_estimators <- unique(standard_estimators)
  if (isTRUE(verbose)) {
    if (length(standard_estimators) == 1L) {
      cli::cli_inform(
        cli::cli_text(
          "Standard-test row uses standard indices for estimator {standard_estimators}."
        )
      )
    } else if (length(standard_estimators) > 1L) {
      cli::cli_inform(
        cli::cli_text(
          "Standard-test rows use standard indices for estimators: {standard_estimators}."
        )
      )
    }
  }

  robust_warning_collector <- lavaan_init_robust_warning_collector(NULL)

  # Apply model_fit to each model in the list
  fit_measures <- Map(
    function(fit, test_value, standard_value, model_label) {
      if (inherits(fit, "lavaan")) {
        model_fit(
          fit,
          type = type,
          metrics = metrics,
          verbose = verbose,
          test = test_value,
          standard_test = standard_value,
          test_details = test_details,
          standard_test_message = FALSE,
          robust_warning_collector = robust_warning_collector,
          robust_warning_message = FALSE,
          model_label = model_label
        )
      } else {
        model_fit(
          fit,
          type = type,
          metrics = metrics,
          verbose = verbose,
          test = test_value,
          standard_test = standard_value,
          test_details = test_details
        )
      }
    },
    fits,
    test_by_model,
    standard_test_by_model,
    model_names
  )

  lavaan_emit_robust_warning(robust_warning_collector, verbose = verbose, message = TRUE)

  # Combine the dataframes vertically
  combined_measures <- do.call(rbind, fit_measures)

  # Add a column to identify each model
  combined_measures$model <- rep(model_names, times = sapply(fit_measures, nrow))

  # Reorder columns so that "model" is the first column
  combined_measures <- combined_measures[, c("model", setdiff(names(combined_measures), "model"))]

  # Upper Text
  names(combined_measures)[1] <- "MODEL"

  # Assign the custom class for print method
  class(combined_measures) <- c("compare_model_fit", class(combined_measures))

  return(combined_measures)
}

is_simple_model_name <- function(name_value) {
  if (length(name_value) == 0L) {
    return(logical(0))
  }
  name_value <- as.character(name_value)
  name_value[is.na(name_value)] <- ""
  grepl("^[A-Za-z.][A-Za-z0-9._]*$", name_value)
}

resolve_model_name_indices <- function(value_names, model_names, alias_names, value_label) {
  if (length(value_names) == 0L) {
    return(integer(0))
  }

  match_indices <- lapply(value_names, function(value_name) {
    which(model_names == value_name |
      (!is.na(alias_names) & alias_names == value_name))
  })

  unknown_names <- value_names[lengths(match_indices) == 0L]
  if (length(unknown_names) > 0L) {
    alias_candidates <- alias_names[!is.na(alias_names)]
    alias_candidates <- alias_candidates[!alias_candidates %in% model_names]
    if (length(alias_candidates) > 0L) {
      alias_counts <- table(alias_candidates)
      alias_candidates <- names(alias_counts[alias_counts == 1L])
    }

    message <- c(
      sprintf(
        "Unknown model name%s in `%s`: %s.",
        if (length(unknown_names) > 1L) "s" else "",
        value_label,
        paste(unknown_names, collapse = ", ")
      ),
      sprintf(
        "Models passed to compare_model_fit(): %s.",
        paste(model_names, collapse = ", ")
      )
    )
    if (length(alias_candidates) > 0L) {
      message <- c(
        message,
        sprintf(
          "You can also use original object names when unambiguous: %s.",
          paste(alias_candidates, collapse = ", ")
        )
      )
    }
    cli::cli_abort(message)
  }

  ambiguous_names <- value_names[lengths(match_indices) > 1L]
  if (length(ambiguous_names) > 0L) {
    ambiguous_name <- ambiguous_names[1]
    ambiguous_models <- model_names[unique(match_indices[[which(value_names == ambiguous_name)[1]]])]
    cli::cli_abort(c(
      sprintf("Model name '%s' in `%s` is ambiguous.", ambiguous_name, value_label),
      sprintf(
        "It matches multiple models: %s.",
        paste(ambiguous_models, collapse = ", ")
      ),
      "Use the argument names shown in the MODEL column to disambiguate."
    ))
  }

  indices <- vapply(match_indices, function(idx) idx[1], integer(1))
  duplicates <- split(value_names, indices)
  duplicates <- duplicates[vapply(duplicates, length, integer(1)) > 1L]
  if (length(duplicates) > 0L) {
    duplicate_index <- as.integer(names(duplicates)[1])
    duplicate_model <- model_names[duplicate_index]
    duplicate_names <- unique(duplicates[[1]])
    cli::cli_abort(c(
      sprintf("`%s` supplies multiple entries for model '%s'.", value_label, duplicate_model),
      sprintf("Use a single name: %s.", paste(duplicate_names, collapse = ", "))
    ))
  }

  indices
}

resolve_test_by_model <- function(test_value, model_names, alias_names, default_value) {
  if (is.list(test_value)) {
    value_names <- names(test_value)
    if (is.null(value_names) || any(value_names == "")) {
      rlang::abort(c(
        "`test` must be a named list when supplying per-model values.",
        sprintf("Valid model names: %s.", paste(model_names, collapse = ", "))
      ))
    }
    for (value_name in value_names) {
      value <- test_value[[value_name]]
      if (is.null(value) || !is.character(value) || length(value) == 0L) {
        rlang::abort(
          sprintf("`test` for model '%s' must be a non-empty character vector.", value_name)
        )
      }
    }
    value_indices <- resolve_model_name_indices(value_names, model_names, alias_names, "test")
    resolved <- rep(list(default_value), length(model_names))
    for (value_idx in seq_along(value_names)) {
      resolved[[value_indices[[value_idx]]]] <- test_value[[value_names[[value_idx]]]]
    }
    resolved
  } else {
    if (!is.null(test_value) && (!is.character(test_value) || length(test_value) == 0L)) {
      rlang::abort("`test` must be a non-empty character vector or \"default\".")
    }
    rep(list(test_value), length(model_names))
  }
}

resolve_standard_test_by_model <- function(standard_value, model_names, alias_names, default_value) {
  if (is.list(standard_value)) {
    value_names <- names(standard_value)
    if (is.null(value_names) || any(value_names == "")) {
      rlang::abort(c(
        "`standard_test` must be a named list when supplying per-model values.",
        sprintf("Valid model names: %s.", paste(model_names, collapse = ", "))
      ))
    }
    for (value_name in value_names) {
      value <- standard_value[[value_name]]
      if (!is.logical(value) || length(value) != 1L) {
        rlang::abort(
          sprintf("`standard_test` for model '%s' must be TRUE or FALSE.", value_name)
        )
      }
    }
    value_indices <- resolve_model_name_indices(
      value_names,
      model_names,
      alias_names,
      "standard_test"
    )
    resolved <- rep(list(default_value), length(model_names))
    for (value_idx in seq_along(value_names)) {
      resolved[[value_indices[[value_idx]]]] <- standard_value[[value_names[[value_idx]]]]
    }
    resolved
  } else {
    if (!is.logical(standard_value) || length(standard_value) != 1L) {
      rlang::abort("`standard_test` must be TRUE or FALSE.")
    }
    rep(list(standard_value), length(model_names))
  }
}
