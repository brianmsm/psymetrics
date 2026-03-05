# Print Method for model_estimates Objects

`print.model_estimates()` prints a text summary of model parameter
estimates to the console in component blocks.

## Usage

``` r
# S3 method for class 'model_estimates'
print(x, digits = 3, ci_digits = digits, p_digits = 3, ...)
```

## Arguments

- x:

  An object of class `model_estimates`.

- digits:

  Number of digits for rounding numeric values. Default is 3.

- ci_digits:

  Number of digits for confidence intervals. Default is `digits`.

- p_digits:

  Number of digits for p-values. Default is 3.

- ...:

  Other arguments are ignored.

## Value

Returns `x` invisibly.

## See also

[`format_results()`](https://brianmsm.github.io/psymetrics/reference/format_results.md)
