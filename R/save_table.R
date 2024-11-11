#' Save a Table to a Specified Format
#'
#' @description
#' `save_table()` exports a table to various formats, with current support for Word documents (.docx).
#' The table is formatted using a specified or default template and can be customized with different font styles,
#' sizes, and alignment options.
#'
#' @param table_data A data frame containing the table data to be exported.
#' @param path A string specifying the file path where the table should be saved. The file extension determines
#'   the format (currently only .docx is supported).
#' @param template A string specifying the path to a Word template (.docx) to be used. If NULL, a default
#'   template provided by the package is used.
#' @param digits An integer indicating the number of decimal places to use when formatting numeric columns.
#'   Defaults to 3.
#' @param ... Additional arguments passed to `flextable::tt` when exporting to Word.
#'
#' @details
#' This function checks the file extension to determine the export format. Currently, only Word documents
#' (.docx) are supported. The function formats the table using the `flextable` package and exports it
#' using the `officer` package. Users can provide a custom Word template or rely on the package's default
#' template.
#'
#' If the `path` has a .docx extension, the table will be saved as a Word document. The default template
#' aligns the table to APA style, and the font is set to Arial with a font size of 10.
#'
#' @examples
#' \dontrun{
#' library(psymetrics)
#'
#' table_data <- data.frame(
#'   Model = c("fit1", "fit2"),
#'   NOBS = c(301, 301),
#'   Chi2 = c(87.13, 24.37),
#'   DF = c(24, 8),
#'   pvalue = c(0.000, 0.002)
#' )
#'
#' save_table(table_data, path = "model_fit.docx")
#' }
#'
#' @export
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
