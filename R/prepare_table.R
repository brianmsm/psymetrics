# Custom function to format numeric columns and apply table formatting
prepare_table <- function(x,
                          columns_to_format = c("NOBS", "NPAR", "Chi2_df"),
                          digits = 3,
                          ci_digits = 3,
                          p_digits = 3,
                          ...) {

  # Round and convert specific columns to character
  common_columns <- intersect(columns_to_format, colnames(x))

  if (length(common_columns) > 0) {
    x[common_columns] <- lapply(x[common_columns], function(col) {
      as.character(round(col, 0))
    })
  }

  # Apply format_table from insight
  formatted_table <- insight::format_table(
    x, digits = digits, ci_digits = digits,
    p_digits = p_digits, ...
  )

  return(formatted_table)
}
