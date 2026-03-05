# Generic Function to Extract Model Parameter Estimates

`model_estimates()` is a generic function that extracts model parameter
estimates from supported model objects. It provides a consistent
interface for CFA/SEM parameter extraction and dispatches to
class-specific methods.

## Usage

``` r
model_estimates(fit, ...)
```

## Arguments

- fit:

  A fitted model object.

- ...:

  Additional arguments passed to class-specific methods.

## Value

A data frame with parameter estimates. For supported classes, the result
includes class `model_estimates`.

## See also

[`model_estimates.lavaan()`](https://brianmsm.github.io/psymetrics/reference/model_estimates.lavaan.md)
for `lavaan` objects and
[`model_fit()`](https://brianmsm.github.io/psymetrics/reference/model_fit.md)
for fit-index extraction.
