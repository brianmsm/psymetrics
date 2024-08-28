# Function to handle the common print logic
print_format <- function(formatted_table, format = "text",
                         align = "firstleft",
                         ...) {

  # Validate the format argument at the start
  # This ensures that we only proceed with supported formats
  valid_formats <- c("text", "html", "markdown", "md")
  if (!format %in% valid_formats) {
    cli::cli_abort(
      c("x" = "Unsupported format {.code {format}}",
        "i" = "Please use {.code text}, {.code html}, {.code markdown} or {.code md}")
    )
  }

  # Common arguments for export_table
  export_args <- list(
    x = formatted_table,
    format = format,
    align = align,
    ...
  )

  # Handle different formats
  if (format == "text") {
    # Use cat for text format to print directly to the console
    cat(do.call(insight::export_table, export_args))
  } else if (format == "html") {
    # Convert to an HTML table using tinytable for HTML format
    return(tinytable::tt(formatted_table, ...))
  } else if (format %in% c("markdown", "md")) {
    # Handle markdown, md
    return(do.call(insight::export_table, export_args))
  }
}

