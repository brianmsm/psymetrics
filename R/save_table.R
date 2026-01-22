#' Save a Table to a Specified Format
#'
#' @description
#' `save_table()` exports a table to various formats, with
#' current support for Word documents (.docx). The table is
#' formatted using a specified or default template and can
#' be customized with different font styles, sizes, and
#' alignment options.
#'
#' @param table_data A data frame containing the table data
#'   to be exported.
#' @param path A string specifying the file path where the
#'   table should be saved. The file extension determines
#'   the format (currently only .docx is supported).
#' @param orientation A character string indicating the
#'   table's orientation when using the default template.
#'   Options are `"landscape"` (default) or `"portrait"`.
#'   `"vertical"` is accepted as an alias for `"portrait"`.
#'   Ignored if a custom template is provided.
#' @param template A string specifying the path to a custom
#'   Word template (.docx). If NULL, a default template
#'   provided by the package is used, with orientation
#'   set by the `orientation` argument.
#' @param digits An integer indicating the number of decimal
#'   places to use when formatting numeric columns.
#'   Defaults to 3.
#' @param ci_digits Number of digits for rounding confidence
#'   intervals. Default is `digits`.
#' @param p_digits Number of digits for rounding p-values.
#'   Default is 3.
#' @param digits_by_col Named integer vector that forces digits for
#'   selected columns. Applied before export.
#'   For `model_fit` and `compare_model_fit`, defaults to
#'   `c(Chi2 = 2, Chi2_df = 2)` when not supplied. When `Chi2_df`
#'   is fractional, it is rounded to two decimals before forming
#'   the `Chi2(df)` header.
#' @param table_args A named list of arguments forwarded to
#'   `prepare_table()` and ultimately `insight::format_table()`.
#'
#' @details
#' This function checks the file extension to determine the
#' export format. Currently, only Word documents (.docx) are
#' supported. The function formats the table using the
#' `flextable` package and exports it using the `officer`
#' package. Users can provide a custom Word template or rely
#' on the package's default template, in either "landscape"
#' or "portrait" orientation as set by the `orientation` argument.
#'
#' If the `path` has a .docx extension, the table will be saved
#' as a Word document. The default template aligns the table
#' to APA style, with Arial font at size 12. If a custom
#' template is provided via the `template` argument, it takes
#' precedence over the `orientation` setting.
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
#'   fit_compared <- compare_model_fit(fit1, fit2)
#'   output_path <- tempfile(fileext = ".docx")
#'   save_table(fit_compared, path = output_path, orientation = "landscape")
#'   save_table(
#'     fit_compared,
#'     path = output_path,
#'     orientation = "landscape",
#'     digits_by_col = c(Chi2 = 2, SRMR = 2)
#'   )
#'   unlink(output_path)
#' } else {
#'   message("Please install 'lavaan' to run this example.")
#' }
save_table <- function(table_data, path, orientation = "landscape",
                       template = NULL, digits = 3, ci_digits = digits,
                       p_digits = 3, digits_by_col = NULL,
                       table_args = list()) {
  # Check if required packages are installed
  rlang::check_installed("tools")

  # Detect file format based on file extension
  file_extension <- tolower(tools::file_ext(path))

  if (file_extension == "docx") {
    format <- "word"
  } else {
    cli::cli_abort("Unsupported file format: {.val {file_extension}}. Currently, only .docx is supported.")
  }

  # Set the default template based on orientation if none is provided
  if (is.null(template)) {
    orientation <- match.arg(orientation, choices = c("landscape", "portrait", "vertical"))
    orientation <- if (orientation == "vertical") "portrait" else orientation
    template <- if (orientation == "landscape") {
      system.file("templates", "template_landscape.docx", package = "psymetrics")
    } else {
      system.file("templates", "template_vertical.docx", package = "psymetrics")
    }
  } else {
    # Use the provided template and inform the user
    cli::cli_inform("Using the provided template: {.file {template}}. The table orientation will follow this template.")
  }

  if (!is.list(table_args)) {
    cli::cli_abort("`table_args` must be a list.")
  }
  if (length(table_args) > 0 &&
      (is.null(names(table_args)) || any(names(table_args) == ""))) {
    cli::cli_abort("`table_args` must use named entries.")
  }

  if (missing(digits_by_col) &&
      inherits(table_data, c("model_fit", "compare_model_fit"))) {
    digits_by_col <- c(Chi2 = 2, Chi2_df = 2)
  }
  if (!is.null(digits_by_col) && "Chi2_df" %in% names(digits_by_col) &&
      "Chi2_df" %in% names(table_data)) {
    df_digits <- digits_by_col[["Chi2_df"]]
    table_data$Chi2_df <- round_df_decimals(table_data$Chi2_df, digits = df_digits)
  }

  base_table_args <- list(
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits
  )
  table_call_args <- utils::modifyList(base_table_args, table_args)
  formatted_table <- do.call(prepare_table, c(list(x = table_data), table_call_args))

  if (!is.null(digits_by_col)) {
    formatted_table <- apply_digits_by_col(formatted_table, digits_by_col = digits_by_col)
  }

  # Create the document and add the table only if the format is Word
  if (format == "word") {
    # Check if required packages are installed
    rlang::check_installed(c("flextable", "officer"))
    # Create a flextable
    ft <- flextable::flextable(formatted_table)

    # Apply formatting options
    ft <- flextable::set_table_properties(ft, width = 1, layout = "autofit") |>
      flextable::font(fontname = "Arial", part = "all") |>
      flextable::fontsize(size = 10, part = "all")
    if (ncol(formatted_table) >= 2) {
      ft <- flextable::align(ft, j = 2:ncol(formatted_table), align = "center", part = "all")
    }
    ft <- ft |>
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
