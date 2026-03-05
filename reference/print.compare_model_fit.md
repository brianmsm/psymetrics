# Print Method for compare_model_fit Objects

`print.compare_model_fit()` prints a text summary of compare-model-fit
tables to the console. By default, `Chi2` is shown with 2 decimals while
other indices use `digits`, and fractional chi-square df values are
rounded to 2 decimals in the `Chi2(df)` header. For markdown or HTML
output (or to override column-specific digits), use
[`format_results()`](https://brianmsm.github.io/psymetrics/reference/format_results.md).

## Usage

``` r
# S3 method for class 'compare_model_fit'
print(x, digits = 3, ci_digits = digits, p_digits = 3, ...)
```

## Arguments

- x:

  An object of class `compare_model_fit`, typically created by the
  [`compare_model_fit`](https://brianmsm.github.io/psymetrics/reference/compare_model_fit.md)
  function.

- digits:

  Number of digits for rounding numeric values. Default is 3.

- ci_digits:

  Number of digits for rounding confidence intervals. Default is
  `digits`.

- p_digits:

  Number of digits for rounding p-values. Default is 3.

- ...:

  Other arguments are ignored.

## Value

Returns `x` invisibly.

## See also

[`format_results()`](https://brianmsm.github.io/psymetrics/reference/format_results.md)

## Examples

``` r
if (requireNamespace("lavaan", quietly = TRUE)) {
  library(psymetrics)
  library(lavaan)

  hs_model <- 'visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9'
  fit1 <- cfa(hs_model, data = HolzingerSwineford1939,
              estimator = "ML")
  fit2 <- cfa(hs_model, data = HolzingerSwineford1939,
              estimator = "MLR")
  comparison <- compare_model_fit(fit1, fit2)
  comparison
  print(comparison, digits = 4)
  html_table <- format_results(comparison, output = "html")
  if (interactive()) {
    html_table
  }
} else {
  message("Please install 'lavaan' to run this example.")
}
#> MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI   |  TLI  
#> -----------------------------------------------------------------------
#> fit1  | 301  |    ML     |  21  |  85.31   |  < .001  | 0.9306 | 0.8958
#> fit2  | 301  |    MLR    |  21  |  87.13   |  < .001  | 0.9252 | 0.8878
#> 
#> MODEL | RMSEA  |    RMSEA  CI     |  SRMR 
#> ------------------------------------------
#> fit1  | 0.0921 | [0.0714, 0.1137] | 0.0652
#> fit2  | 0.0935 | [0.0726, 0.1152] | 0.0652
#> 
```
