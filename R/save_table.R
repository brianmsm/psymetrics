save_table <- function(table_data, path, template = NULL, digits = 3, ...) {
  # Check if required packages are installed
  rlang::check_installed("tools")

  # Detect file format based on file extension
  file_extension <- tools::file_ext(path)

  if (file_extension == "docx") {
    format <- "word"
  } else {
    cli::cli_abort("Unsupported file format: {.val {file_extension}}. Currently, only .docx is supported.")
  }

  # Set the default template if none is provided
  if (is.null(template)) {
    template <- system.file("templates", "template_vertical.docx", package = "psymetrics")
  }

  table_data <- prepare_table(table_data, digits = digits)

  # Create the document and add the table only if the format is Word
  if (format == "word") {
    # Check if required packages are installed
    rlang::check_installed(c("flextable", "officer"))
    # Create a flextable
    ft <- flextable::flextable(table_data)

    # Apply formatting options
    ft <- flextable::set_table_properties(ft, width = 1, layout = "autofit") |>
      flextable::font(fontname = "Arial", part = "all") |>
      flextable::fontsize(size = 10, part = "all") |>
      flextable::align(j = 2:ncol(table_data), align = "center", part = "all") |>
      flextable::align(j = 1, align = "left", part = "all") |>
      flextable::border_remove() |>
      flextable::hline_top(part = "all",
                           border = officer::fp_border(color = "black", width = 0.5)) |>
      flextable::hline_bottom(part = "body",
                              border = officer::fp_border(color = "black", width = 0.5))

    # Read the Word template
    doc <- officer::read_docx(template) |>
      flextable::body_add_flextable(value = ft)

    # Save the document
    print(doc, target = path)
    cli::cli_inform("Table successfully saved as a Word document at {.file {path}}.")
  }

  invisible(path)
}
