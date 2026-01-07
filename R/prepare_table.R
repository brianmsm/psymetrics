#' Prepare a formatted table for export (internal).
#'
#' @description
#' Internal S3 generic used by formatting helpers. It delegates to
#' class-specific methods that call `insight::format_table()`.
#'
#' @param x Object containing the table data.
#' @param digits,ci_digits,p_digits Numeric formatting controls applied
#'   by `insight::format_table()`.
#' @param ... Additional arguments forwarded to `insight::format_table()`.
#'
#' @return A formatted data frame (character columns ready for export).
#' @keywords internal
#' @noRd
prepare_table <- function(x, ...) {
  UseMethod("prepare_table")
}

#' @keywords internal
#' @noRd
prepare_table.data.frame <- function(x, digits = 3, ci_digits = digits,
                                     p_digits = 3, ...) {
  insight::format_table(
    x,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ...
  )
}
