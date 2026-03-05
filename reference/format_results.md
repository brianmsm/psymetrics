# Format model results for output

`format_results()` prepares a base table and exports it to text,
markdown, or HTML. Use `output = "auto"` to choose markdown or HTML
based on the rendering context; this is a best-effort heuristic, so set
`output` explicitly to override.

## Usage

``` r
format_results(
  x,
  output = c("auto", "text", "markdown", "md", "html"),
  digits = 3,
  ci_digits = digits,
  p_digits = 3,
  align = "firstleft",
  digits_by_col = NULL,
  table_args = list(),
  output_args = list()
)
```

## Arguments

- x:

  An object containing table data.

- output:

  Output format. One of "auto", "text", "markdown", "md", or "html".

- digits:

  An integer indicating the number of decimal places to use when
  formatting numeric columns. Defaults to 3.

- ci_digits:

  Number of digits for rounding confidence intervals. Default is
  `digits`.

- p_digits:

  Number of digits for rounding p-values. Default is 3.

- align:

  Alignment for text or markdown output (ignored for HTML). Passed to
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html).
  Defaults to `"firstleft"`, with options like `"left"`, `"right"`, or
  `"center"`, or custom specifications such as `"lccrl"`. See
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html)
  for details.

- digits_by_col:

  Named integer vector that forces digits for selected columns. Applied
  before export. For `model_fit` and `compare_model_fit`, defaults to
  `c(Chi2 = 2, Chi2_df = 2)` when not supplied. When `Chi2_df` is
  fractional, it is rounded to two decimals before forming the
  `Chi2(df)` header.

- table_args:

  A named list of arguments forwarded to
  [`insight::format_table()`](https://easystats.github.io/insight/reference/format_table.html).

- output_args:

  A named list of arguments forwarded to
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html)
  or
  [`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html)
  depending on `output`. If `align` is supplied here and the top-level
  `align` argument is missing, `output_args$align` is used. Supplying
  `align` in both places is an error. `output_args$format` is not
  allowed; use `output` instead.

## Value

A character string (text), a `knitr_kable` (markdown), or a `tinytable`
(HTML).

## Note

HTML output returns a `tinytable` object. Printing HTML tables inside
RStudio requires the `rstudioapi` package; you can still create the
object without it, but printing will error unless `rstudioapi` is
installed.

## Examples

``` r
if (requireNamespace("lavaan", quietly = TRUE)) {
  library(lavaan)
  library(psymetrics)

  hs_model <- 'visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9'

  fit <- cfa(hs_model, data = HolzingerSwineford1939, estimator = "MLR")
  results <- model_fit(fit)
  format_results(results, output = "text")
  format_results(results, output = "text", digits = 2, p_digits = 4)
  format_results(
    results,
    output = "text",
    digits_by_col = c(Chi2 = 2, SRMR = 2)
  )
  format_results(
    results,
    output = "text",
    table_args = list(ci_brackets = FALSE)
  )
  if (requireNamespace("knitr", quietly = TRUE)) {
    format_results(
      results,
      output = "markdown",
      align = "center",
      output_args = list(caption = "Fit indices")
    )
    format_results(
      results,
      output = "markdown",
      output_args = list(align = "center", caption = "Fit indices")
    )
  }
  html_table <- format_results(results, output = "html")
  if (interactive()) {
    html_table
  }
} else {
  message("Please install 'lavaan' to run this example.")
}
```
