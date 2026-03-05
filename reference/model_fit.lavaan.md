# Extract Fit Indices from a lavaan Model

`model_fit.lavaan` extracts fit indices from a `lavaan` model object
from CFA, SEM, growth, multigroup, and related workflows. The function
allows you to specify the type of indices to extract: `"standard"`,
`"scaled"`, or `"robust"`. If the model uses a robust estimator and you
specify `type = "scaled"` or `type = "robust"`, the corresponding
indices will be returned. If no type is specified, the function
automatically chooses `"scaled"` for robust estimators and `"standard"`
otherwise. When the model was fitted with multiple tests, the function
can return multiple rows (one per non-standard test).

## Usage

``` r
# S3 method for class 'lavaan'
model_fit(
  fit,
  type = NULL,
  metrics = "essential",
  verbose = TRUE,
  test = "default",
  standard_test = FALSE,
  test_details = FALSE,
  ...
)
```

## Arguments

- fit:

  A fitted `lavaan` object (for example from
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html),
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html),
  [`lavaan::growth()`](https://rdrr.io/pkg/lavaan/man/growth.html), and
  related functions).

- type:

  A character string specifying the type of fit indices to extract.
  Options are `"standard"`, `"scaled"`, and `"robust"`. Defaults to
  `NULL`, which will automatically choose `"scaled"` if a robust
  estimator is used; otherwise `"standard"`.

- metrics:

  A character vector specifying the fit indices to return. The default
  is `"essential"`, which includes common fit indices. You can also
  specify a custom set of metrics.

- verbose:

  A logical value indicating whether to display informational messages
  about metric adjustments. Defaults to `TRUE`.

- test:

  A character string or vector specifying which lavaan tests to report.
  Use `"default"` (the default) to return all non-standard tests from
  `lavInspect(fit, "options")$test`, excluding `"standard"`,
  `"default"`, and `"none"`. Provide a character vector to request
  specific tests; unavailable entries are dropped with an informational
  message when `verbose = TRUE`. `NULL` is treated as `"default"`.

- standard_test:

  A logical value indicating whether to include a standard-test row in
  addition to non-standard tests. When `TRUE`, the standard row is shown
  first and always uses standard indices.

- test_details:

  A logical value indicating whether to add `TEST` and `SE` columns
  describing the test and standard error settings used to compute the
  fit measures. When `standard_test = TRUE` adds the standard row, `SE`
  is set to `NA` for that row because fit indices do not depend on
  standard errors. When `FALSE` (default), `ESTIMATOR` substitutes the
  test name for unknown estimator variants.

- ...:

  Additional arguments passed to methods.

## Value

A data frame containing the specified fit indices of the model. When
multiple tests are reported, the data frame can include multiple rows.

## See also

[model_fit](https://brianmsm.github.io/psymetrics/reference/model_fit.md)
for an overview of model fit methods in the package.

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
  model_fit(fit)
  sem_model <- 'visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9
                textual ~ visual
                speed ~ textual'
  fit_sem <- sem(sem_model, data = HolzingerSwineford1939)
  model_fit(fit_sem)
  model_fit(fit, standard_test = TRUE, test_details = TRUE)
  model_fit(fit, type = "robust")
  model_fit(fit, metrics = c("cfi", "tli"))
  fit_test <- cfa(
    hs_model,
    data = HolzingerSwineford1939,
    test = c("satorra.bentler", "mean.var.adjusted")
  )
  model_fit(fit_test, test = "satorra.bentler")
} else {
  message("Please install 'lavaan' to run this example.")
}
#> Standard-test row uses standard indices for estimator ML.
#> cfi and tli were adjusted to their scaled version.
#> If you want to control the specific metric type used, specify it explicitly
#> (e.g., `cfi.robust`) or modify the type argument.
#> NOBS |    ESTIMATOR    | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> ---------------------------------------------------------------------------
#> 301  | satorra.bentler |  21  |  80.87   |  < .001  | 0.925 | 0.887 | 0.089
#> 
#> NOBS |   RMSEA  CI    | SRMR 
#> -----------------------------
#> 301  | [0.068, 0.110] | 0.065
#> 
```
