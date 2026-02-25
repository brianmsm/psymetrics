#' Apply per-column digit overrides to formatted tables (internal).
#'
#' @param x A formatted data frame or a list of formatted data frames.
#' @param digits_by_col Named integer vector that forces digits for
#'   selected columns. Matching is exact, then by prefix, and only pure
#'   numeric cells are modified.
#'
#' @return An object of the same shape as `x` with digits applied.
#' @keywords internal
#' @noRd
apply_digits_by_col <- function(x, digits_by_col) {
  if (is.null(digits_by_col) || length(digits_by_col) == 0) {
    return(x)
  }

  digits_by_col <- unlist(digits_by_col, use.names = TRUE)
  if (is.null(names(digits_by_col)) || any(names(digits_by_col) == "")) {
    cli::cli_abort("`digits_by_col` must be a named numeric vector.")
  }
  if (!is.numeric(digits_by_col) || any(!is.finite(digits_by_col))) {
    cli::cli_abort("`digits_by_col` must contain finite numeric values.")
  }
  if (any(digits_by_col %% 1 != 0)) {
    cli::cli_abort("`digits_by_col` must contain whole-number digits.")
  }

  apply_to_df <- function(df) {
    column_names <- names(df)
    for (col_key in names(digits_by_col)) {
      target_cols <- if (col_key %in% column_names) {
        col_key
      } else {
        column_names[startsWith(column_names, col_key)]
      }

      if (length(target_cols) == 0) {
        next
      }

      digits <- as.integer(digits_by_col[[col_key]])
      for (col_name in target_cols) {
        df[[col_name]] <- format_numeric_cells(df[[col_name]], digits = digits)
      }
    }

    df
  }

  if (is.data.frame(x)) {
    return(apply_to_df(x))
  }

  if (is.list(x)) {
    # If we need to support more container types in the future,
    # consider splitting this dispatcher into dedicated handlers.
    return(lapply(x, function(block) {
      if (is.data.frame(block)) {
        apply_to_df(block)
      } else {
        block
      }
    }))
  }

  x
}

#' Format numeric-only character cells to a fixed number of decimals.
#'
#' @param values Character vector to format.
#' @param digits Integer number of decimals to apply.
#'
#' @return A character vector with numeric-only cells formatted.
#' @keywords internal
#' @noRd
format_numeric_cells <- function(values, digits) {
  values <- as.character(values)
  trimmed <- trimws(values)
  numeric_mask <- !is.na(trimmed) &
    grepl("^[+-]?(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][+-]?\\d+)?$", trimmed)

  if (any(numeric_mask)) {
    values[numeric_mask] <- formatC(
      as.numeric(trimmed[numeric_mask]),
      format = "f",
      digits = digits
    )
  }

  values
}

#' Drop converged column when it provides no additional signal.
#'
#' @param x A data frame with formatted values.
#'
#' @return A data frame with `converged` removed when all TRUE/NA.
#' @keywords internal
#' @noRd
drop_converged_column <- function(x) {
  if (!"converged" %in% names(x)) {
    return(x)
  }

  converged_vals <- x$converged
  if (all(is.na(converged_vals) | converged_vals)) {
    x$converged <- NULL
  }

  x
}

#' Round non-integer degrees of freedom values for Chi2 headers.
#'
#' @param values Numeric vector with df values.
#' @param digits Integer number of decimals to keep for non-integers.
#'
#' @return Numeric vector with rounded non-integer values.
#' @keywords internal
#' @noRd
round_df_decimals <- function(values, digits = 2) {
  if (!is.numeric(values)) {
    return(values)
  }

  decimal_mask <- !is.na(values) & values %% 1 != 0
  if (any(decimal_mask)) {
    values[decimal_mask] <- round(values[decimal_mask], digits = digits)
  }

  values
}
