# Print Method for Model Fit Indices

`print.model_fit()` prints a text summary of model fit tables to the
console. By default, `Chi2` is shown with 2 decimals while other indices
use `digits`, and fractional chi-square df values are rounded to 2
decimals in the `Chi2(df)` header. For markdown or HTML output (or to
override column-specific digits), use
[`format_results()`](https://brianmsm.github.io/psymetrics/reference/format_results.md).

## Usage

``` r
# S3 method for class 'model_fit'
print(x, digits = 3, ci_digits = digits, p_digits = 3, ...)
```

## Arguments

- x:

  An object of class `model_fit`, typically created by the
  [`model_fit`](https://brianmsm.github.io/psymetrics/reference/model_fit.md)
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
  library(lavaan)
  library(psymetrics)

  hs_model <- 'visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9'

  fit <- cfa(hs_model, data = HolzingerSwineford1939,
             estimator = "MLR")
  result <- model_fit(fit)
  result
  print(result, digits = 4)
  if (requireNamespace("knitr", quietly = TRUE)) {
    format_results(result, output = "markdown")
  }
} else {
  message("Please install 'lavaan' to run this example.")
}
#> NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI   |  TLI   | RMSEA 
#> ------------------------------------------------------------------------
#> 301  |    MLR    |  21  |  87.13   |  < .001  | 0.9252 | 0.8878 | 0.0935
#> 
#> NOBS |    RMSEA  CI     |  SRMR 
#> --------------------------------
#> 301  | [0.0726, 0.1152] | 0.0652
#> 
#> 
#> 
#> |NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |   CFI |   TLI | RMSEA |      RMSEA  CI |  SRMR |
#> |:----|:---------:|:----:|:--------:|:--------:|:-----:|:-----:|:-----:|:--------------:|:-----:|
#> |301  |       MLR |   21 |    87.13 |   < .001 | 0.925 | 0.888 | 0.093 | [0.073, 0.115] | 0.065 |
```
