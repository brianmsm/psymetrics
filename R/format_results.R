#' Format model results for output
#'
#' @description
#' `format_results()` prepares a base table and exports it 
#' to text, markdown, or HTML. Use `output = "auto"` to choose
#' markdown or HTML based on the rendering context; this is a
#' best-effort heuristic, so set `output` explicitly to override.
#'
#' @param x An object containing table data.
#' @param output Output format. One of "auto", "text", "markdown",
#'   "md", or "html".
#' @param digits An integer indicating the number of decimal
#'   places to use when formatting numeric columns.
#'   Defaults to 3.
#' @param ci_digits Number of digits for rounding confidence
#'   intervals. Default is `digits`.
#' @param p_digits Number of digits for rounding p-values.
#'   Default is 3.
#' @param align Alignment for text or markdown output (ignored for HTML).
#'   Passed to `insight::export_table()`. Defaults to `"firstleft"`, with
#'   options like `"left"`, `"right"`, or `"center"`, or custom
#'   specifications such as `"lccrl"`. See `insight::export_table()`
#'   for details.
#' @param digits_by_col Named integer vector that forces digits for
#'   selected columns. Applied before export.
#'   For `model_fit` and `compare_model_fit`, defaults to
#'   `c(Chi2 = 2, Chi2_df = 2)` when not supplied. When `Chi2_df`
#'   is fractional, it is rounded to two decimals before forming
#'   the `Chi2(df)` header.
#' @param table_args A named list of arguments forwarded to
#'   `insight::format_table()`.
#' @param output_args A named list of arguments forwarded to
#'   `insight::export_table()` or `tinytable::tt()` depending on
#'   `output`. If `align` is supplied here and the top-level `align`
#'   argument is missing, `output_args$align` is used. Supplying `align`
#'   in both places is an error. `output_args$format` is not allowed;
#'   use `output` instead.
#' @note HTML output returns a `tinytable` object. Printing HTML
#'   tables inside RStudio requires the `rstudioapi` package; you
#'   can still create the object without it, but printing will
#'   error unless `rstudioapi` is installed.
#'
#' @return A character string (text), a `knitr_kable` (markdown), or a
#'   `tinytable` (HTML).
#' @export
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   library(lavaan)
#'   library(psymetrics)
#'
#'   hs_model <- 'visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9'
#'
#'   fit <- cfa(hs_model, data = HolzingerSwineford1939, estimator = "MLR")
#'   results <- model_fit(fit)
#'   format_results(results, output = "text")
#'   format_results(results, output = "text", digits = 2, p_digits = 4)
#'   format_results(
#'     results,
#'     output = "text",
#'     digits_by_col = c(Chi2 = 2, SRMR = 2)
#'   )
#'   format_results(
#'     results,
#'     output = "text",
#'     table_args = list(ci_brackets = FALSE)
#'   )
#'   if (requireNamespace("knitr", quietly = TRUE)) {
#'     format_results(
#'       results,
#'       output = "markdown",
#'       align = "center",
#'       output_args = list(caption = "Fit indices")
#'     )
#'     format_results(
#'       results,
#'       output = "markdown",
#'       output_args = list(align = "center", caption = "Fit indices")
#'     )
#'   }
#'   html_table <- format_results(results, output = "html")
#'   if (interactive()) {
#'     html_table
#'   }
#' } else {
#'   message("Please install 'lavaan' to run this example.")
#' }
format_results <- function(x,
                           output = c("auto", "text", "markdown", "md", "html"),
                           digits = 3,
                           ci_digits = digits,
                           p_digits = 3,
                           align = "firstleft",
                           digits_by_col = NULL,
                           table_args = list(),
                           output_args = list()) {
  UseMethod("format_results")
}

#' @export
format_results.data.frame <- function(x,
                                      output = c("auto", "text", "markdown", "md", "html"),
                                      digits = 3,
                                      ci_digits = digits,
                                      p_digits = 3,
                                      align = "firstleft",
                                      digits_by_col = NULL,
                                      table_args = list(),
                                      output_args = list()) {
  format_results_impl(
    x = x,
    output = output,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    align = align,
    digits_by_col = digits_by_col,
    table_args = table_args,
    output_args = output_args,
    missing_digits = missing(digits),
    missing_ci_digits = missing(ci_digits),
    missing_p_digits = missing(p_digits),
    missing_align = missing(align)
  )
}

#' @keywords internal
#' @noRd
format_results_impl <- function(x,
                                output,
                                digits,
                                ci_digits,
                                p_digits,
                                align,
                                digits_by_col,
                                table_args,
                                output_args,
                                missing_digits,
                                missing_ci_digits,
                                missing_p_digits,
                                missing_align) {
  if (!is.list(table_args)) {
    cli::cli_abort("`table_args` must be a list.")
  }
  if (length(table_args) > 0 &&
      (is.null(names(table_args)) || any(names(table_args) == ""))) {
    cli::cli_abort("`table_args` must use named entries.")
  }

  if (!is.list(output_args)) {
    cli::cli_abort("`output_args` must be a list.")
  }
  if (length(output_args) > 0 &&
      (is.null(names(output_args)) || any(names(output_args) == ""))) {
    cli::cli_abort("`output_args` must use named entries.")
  }

  core_args <- c("digits", "ci_digits", "p_digits")
  passed_direct <- !missing_digits || !missing_ci_digits || !missing_p_digits
  if (passed_direct && any(core_args %in% names(table_args))) {
    cli::cli_abort(
      "Do not pass `digits/ci_digits/p_digits` in both places; use only direct arguments or only `table_args`."
    )
  }

  output <- match.arg(output, c("auto", "text", "markdown", "md", "html"))
  if (output == "md") {
    output <- "markdown"
  }
  output <- .resolve_output_auto(output)

  base_table_args <- list(
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits
  )
  table_call_args <- utils::modifyList(base_table_args, table_args)
  formatted_table <- do.call(prepare_table, c(list(x = x), table_call_args))

  if (!is.null(digits_by_col)) {
    formatted_table <- apply_digits_by_col(formatted_table, digits_by_col = digits_by_col)
  }

  if ("format" %in% names(output_args)) {
    cli::cli_abort("`output_args$format` is not supported. Use the top-level `output` argument.")
  }
  if ("align" %in% names(output_args)) {
    if (!missing_align) {
      cli::cli_abort(
        "Do not pass `align` in both places; use either the top-level `align` argument or `output_args$align`."
      )
    }
    align <- output_args[["align"]]
    output_args$align <- NULL
  }

  if (output == "text") {
    return(do.call(
      insight::export_table,
      c(list(formatted_table, format = "text", align = align), output_args)
    ))
  }

  if (output == "markdown") {
    return(do.call(
      insight::export_table,
      c(list(formatted_table, format = "markdown", align = align), output_args)
    ))
  }

  table_html <- do.call(tinytable::tt, c(list(formatted_table), output_args))
  if (methods::is(table_html, "tinytable")) {
    methods::slot(table_html, "output") <- "html"
  }
  table_html
}

#' @keywords internal
#' @noRd
.resolve_output_auto <- function(output) {
  if (output != "auto") {
    return(output)
  }

  if (interactive()) {
    return("html")
  }

  if (requireNamespace("knitr", quietly = TRUE)) {
    target <- knitr::opts_knit$get("rmarkdown.pandoc.to")
    if (!is.null(target) && length(target) > 0 &&
        any(grepl("html", target, ignore.case = TRUE))) {
      return("html")
    }
  }

  "markdown"
}
