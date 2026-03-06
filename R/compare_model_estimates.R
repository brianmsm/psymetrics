#' Compare Parameter Estimates Across Multiple Models
#'
#' @description
#' `compare_model_estimates()` compares parameter estimates from two or more
#' fitted models. It extracts estimates via [model_estimates()] and aligns
#' parameters by their structural identity so that shared and model-specific
#' parameters can be displayed side by side.
#'
#' The current implementation is designed for fitted `lavaan` objects and
#' integrates with the package formatting and export workflow through
#' `prepare_table()`, [format_results()], and [save_table()].
#'
#' @param ... Two or more fitted model objects to compare.
#' @param standardized Controls standardized extraction. Accepted values are
#'   `FALSE`, `TRUE`, `"std.all"`, `"std.lv"`, `"std.nox"`, plus aliases:
#'   `"all"`, `"latent"`, `"lv"`, and `"no_exogenous"`.
#' @param ci Confidence level for interval columns. Must be a numeric value in
#'   `(0, 1)`.
#' @param component Character vector indicating which parameter components to
#'   keep. Use `"all"` (default) to keep all available components.
#' @param verbose Logical; if `TRUE`, prints informative messages emitted by
#'   the underlying extraction methods.
#'
#' @return A data frame with class `compare_model_estimates`. The result
#'   contains parameter identity columns (`Group`, `Level`, `Component`, `To`,
#'   `Operator`, `From`) plus model-specific estimate columns such as
#'   `Coefficient.<model>`, `SE.<model>`, `CI_low.<model>`, `CI_high.<model>`,
#'   `z.<model>`, `p.<model>`, and `converged.<model>`.
#' @seealso [model_estimates()] for single-model extraction and
#'   [format_results()] for output customization.
#' @export
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'
#'   hs_cfa_model <- '
#'     visual =~ x1 + x2 + x3
#'     textual =~ x4 + x5 + x6
#'     speed =~ x7 + x8 + x9
#'   '
#'
#'   hs_sem_model <- '
#'     visual =~ x1 + x2 + x3
#'     textual =~ x4 + x5 + x6
#'     speed =~ x7 + x8 + x9
#'     textual ~ visual
#'     speed ~ visual + textual
#'   '
#'
#'   fit1 <- cfa(hs_cfa_model, data = HolzingerSwineford1939)
#'   fit2 <- sem(hs_sem_model, data = HolzingerSwineford1939)
#'
#'   compared <- compare_model_estimates(CFA = fit1, SEM = fit2)
#'   compared
#'   format_results(compared, table_args = list(select = "ci_p2"))
#' } else {
#'   message("Please install 'lavaan' to run this example.")
#' }
compare_model_estimates <- function(...,
                                    standardized = FALSE,
                                    ci = 0.95,
                                    component = "all",
                                    verbose = TRUE) {
  fits <- list(...)

  if (length(fits) < 2) {
    cli::cli_abort("At least two model fits must be provided for comparison.")
  }

  arg_names <- names(fits)
  deparsed_names <- vapply(
    substitute(list(...))[-1L],
    function(x) paste(deparse(x), collapse = " "),
    character(1)
  )
  if (!is.null(arg_names)) {
    model_names <- ifelse(arg_names != "", arg_names, deparsed_names)
  } else {
    model_names <- deparsed_names
  }
  model_names <- compare_model_estimates_unique_names(model_names, verbose = verbose)

  estimates_list <- Map(
    function(fit, model_name) {
      estimates <- model_estimates(
        fit,
        standardized = standardized,
        ci = ci,
        component = component,
        verbose = verbose
      )
      compare_model_estimates_validate_keys(estimates, model_name = model_name)
      estimates
    },
    fits,
    model_names
  )

  compared <- compare_model_estimates_build_table(
    estimates_list = estimates_list,
    model_names = model_names
  )

  class(compared) <- c("compare_model_estimates", class(compared))
  attr(compared, "ci") <- ci
  attr(compared, "standardized") <- standardized
  attr(compared, "model_names") <- model_names
  compared
}


# Helpers -----------------------------------------------------------------

compare_model_estimates_unique_names <- function(model_names, verbose = TRUE) {
  if (!is.character(model_names) || length(model_names) == 0L) {
    return(model_names)
  }

  unique_names <- make.unique(model_names, sep = "_")
  if (isTRUE(verbose) && !identical(unique_names, model_names)) {
    changed <- unique_names != model_names
    rename_pairs <- paste0(model_names[changed], " -> ", unique_names[changed])
    cli::cli_inform(
      "Duplicate model names were made unique for display: {rename_pairs}."
    )
  }

  unique_names
}

compare_model_estimates_validate_keys <- function(estimates, model_name) {
  key_cols <- c("Group", "Level", "Component", "To", "Operator", "From")
  estimates_df <- compare_model_estimates_normalize_estimates(estimates)
  key_id <- compare_model_estimates_make_key_id(estimates_df[key_cols])

  if (anyDuplicated(key_id)) {
    duplicate_rows <- which(duplicated(key_id) | duplicated(key_id, fromLast = TRUE))
    duplicate_links <- model_estimates_compose_link(
      to = estimates_df$To[duplicate_rows],
      operator = estimates_df$Operator[duplicate_rows],
      from = estimates_df$From[duplicate_rows]
    )
    duplicate_links <- unique(duplicate_links[!is.na(duplicate_links) & nzchar(duplicate_links)])

    cli::cli_abort(c(
      sprintf(
        "Model '%s' returned duplicated parameter rows that cannot be aligned for comparison.",
        model_name
      ),
      if (length(duplicate_links) > 0L) {
        sprintf("Affected parameter links: %s.", paste(duplicate_links, collapse = ", "))
      }
    ))
  }

  invisible(estimates)
}

compare_model_estimates_build_table <- function(estimates_list, model_names) {
  normalized <- lapply(estimates_list, compare_model_estimates_normalize_estimates)
  key_cols <- c("Group", "Level", "Component", "To", "Operator", "From")
  value_cols <- c("Coefficient", "SE", "CI_low", "CI_high", "z", "p", "converged")

  key_frames <- lapply(normalized, function(x) x[key_cols])
  all_keys <- do.call(rbind, key_frames)
  if (nrow(all_keys) == 0L) {
    union_keys <- all_keys
  } else {
    all_key_ids <- compare_model_estimates_make_key_id(all_keys)
    union_keys <- all_keys[!duplicated(all_key_ids), , drop = FALSE]
    rownames(union_keys) <- NULL
  }

  out <- union_keys
  union_key_ids <- compare_model_estimates_make_key_id(union_keys)

  for (idx in seq_along(normalized)) {
    estimates_df <- normalized[[idx]]
    model_name <- model_names[[idx]]
    estimate_ids <- compare_model_estimates_make_key_id(estimates_df[key_cols])
    match_idx <- match(union_key_ids, estimate_ids)

    for (value_col in value_cols) {
      column_name <- paste0(value_col, ".", model_name)
      if (nrow(estimates_df) == 0L) {
        out[[column_name]] <- compare_model_estimates_missing_value(value_col, nrow(out))
      } else {
        out[[column_name]] <- estimates_df[[value_col]][match_idx]
      }
    }
  }

  out
}

compare_model_estimates_normalize_estimates <- function(estimates) {
  estimates_df <- as.data.frame(estimates, stringsAsFactors = FALSE)
  key_cols <- c("Group", "Level", "Component", "To", "Operator", "From")
  value_cols <- c("Coefficient", "SE", "CI_low", "CI_high", "z", "p", "converged")

  for (col_name in key_cols) {
    if (!col_name %in% names(estimates_df)) {
      estimates_df[[col_name]] <- rep(NA_character_, nrow(estimates_df))
    }
    estimates_df[[col_name]] <- as.character(estimates_df[[col_name]])
  }

  for (col_name in value_cols) {
    if (!col_name %in% names(estimates_df)) {
      estimates_df[[col_name]] <- compare_model_estimates_missing_value(
        col_name,
        nrow(estimates_df)
      )
    }
  }

  estimates_df[, c(key_cols, value_cols), drop = FALSE]
}

compare_model_estimates_missing_value <- function(column_name, size) {
  if (identical(column_name, "converged")) {
    return(rep(NA, size))
  }

  rep(NA_real_, size)
}

compare_model_estimates_make_key_id <- function(key_df) {
  if (!is.data.frame(key_df) || nrow(key_df) == 0L) {
    return(character(0))
  }

  normalized <- lapply(key_df, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "<NA>"
    x
  })

  do.call(paste, c(normalized, sep = "\r"))
}

