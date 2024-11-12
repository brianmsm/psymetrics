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
#' @param orientation A character string indicating the table's orientation when using the default template.
#'   Options are `"landscape"` (default) or `"portrait"`. Ignored if a custom template is provided.
#' @param template A string specifying the path to a custom Word template (.docx). If NULL, a default
#'   template provided by the package is used, with orientation set by the `orientation` argument.
#' @param digits An integer indicating the number of decimal places to use when formatting numeric columns.
#'   Defaults to 3.
#' @details
#' This function checks the file extension to determine the export format. Currently, only Word documents
#' (.docx) are supported. The function formats the table using the `flextable` package and exports it
#' using the `officer` package. Users can provide a custom Word template or rely on the package's default
#' template, in either "landscape" or "portrait" orientation as set by the `orientation` argument.
#'
#' If the `path` has a .docx extension, the table will be saved as a Word document. The default template
#' aligns the table to APA style, with Arial font at size 12. If a custom template is provided via the
#' `template` argument, it takes precedence over the `orientation` setting.
#'
#' @examples
#' library(lavaan)
#' library(psymetrics)
#' model1 <- 'visual  =~ x1 + x2 + x3 + x4'
#' model2 <- 'visual  =~ x1 + x2 + x3 + x4 + x5'
#' fit1 <- cfa(model1, data = HolzingerSwineford1939, estimator = "MLR")
#' fit2 <- cfa(model2, data = HolzingerSwineford1939, estimator = "MLR")
#' fit_compared <- compare_model_fit(fit1, fit2)
#' save_table(fit_compared, path = "model_fit.docx", orientation = "landscape")
#'
#' @export

save_table <- function(table_data, path, orientation = "landscape",
                       template = NULL, digits = 3, ...) {
  # Check if required packages are installed
  rlang::check_installed("tools")

  # Detect file format based on file extension
  file_extension <- tools::file_ext(path)

  if (file_extension == "docx") {
    format <- "word"
  } else {
    cli::cli_abort("Unsupported file format: {.val {file_extension}}. Currently, only .docx is supported.")
  }

  # Set the default template based on orientation if none is provided
  if (is.null(template)) {
    orientation <- match.arg(orientation, choices = c("landscape", "vertical"))
    template <- if (orientation == "landscape") {
      system.file("templates", "template_landscape.docx", package = "psymetrics")
    } else {
      system.file("templates", "template_vertical.docx", package = "psymetrics")
    }
  } else {
    # Use the provided template and inform the user
    cli::cli_inform("Using the provided template: {.file {template}}. The table orientation will follow this template.")
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
