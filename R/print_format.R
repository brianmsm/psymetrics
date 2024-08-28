# Function to handle the common print logic
print_format <- function(formatted_table, format = "text",
                         digits = 3, ...) {
  if (format == "text") {
    cat(
      insight::export_table(
        x = formatted_table,
        digits = digits,
        format = format,
        ...
      )
    )
  } else if (format == "html") {
    return(tinytable::tt(formatted_table, digits = digits, ...))
  } else if (format %in% c("markdown", "md")) {
    insight::export_table(
      x = formatted_table,
      digits = digits,
      format = format,
      ...
    )
  }
}
