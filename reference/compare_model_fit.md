# Compare Model Fit Indices Across Multiple Models

`compare_model_fit()` compares the fit indices of two or more models. It
extracts the fit indices using
[`model_fit()`](https://brianmsm.github.io/psymetrics/reference/model_fit.md)
and combines them into a single data frame for easy comparison. It
supports any fitted `lavaan` model objects (including CFA, SEM, growth,
multigroup, and multilevel variants).

## Usage

``` r
compare_model_fit(
  ...,
  type = NULL,
  metrics = "essential",
  verbose = TRUE,
  test = "default",
  standard_test = FALSE,
  test_details = FALSE
)
```

## Arguments

- ...:

  Two or more model objects to be compared. For `lavaan`, pass any
  fitted objects created with
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html),
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html),
  [`lavaan::growth()`](https://rdrr.io/pkg/lavaan/man/growth.html), or
  related functions.

- type:

  A character string specifying the type of fit indices to extract.
  Options are `"standard"`, `"scaled"`, and `"robust"`. Defaults to
  `NULL`, which automatically selects `"scaled"` if a robust estimator
  is used, otherwise `"standard"`.

- metrics:

  A character vector specifying which fit indices to extract. Defaults
  to `"essential"`, or a custom vector of indices.

- verbose:

  Logical. If `TRUE`, prints messages about the indices being adjusted.

- test:

  A character string or vector specifying which lavaan tests to report.
  Use `"default"` (the default) to return all non-standard tests from
  `lavInspect(fit, "options")$test`, excluding `"standard"`,
  `"default"`, and `"none"`. Provide a character vector to request
  specific tests; unavailable entries are dropped with an informational
  message when `verbose = TRUE`. When supplying a list, it must be named
  and each name must match the model labels shown in the `MODEL` column.
  If you supply named arguments, you may also reference the original
  object names when they are unambiguous.

- standard_test:

  A logical value indicating whether to include a standard-test row in
  addition to non-standard tests. When `TRUE`, the standard row is shown
  first and always uses standard indices. When supplying a list, it must
  be named and each name must match the model labels shown in the
  `MODEL` column. If you supply named arguments, you may also reference
  the original object names when they are unambiguous.

- test_details:

  Logical. If `TRUE`, include `TEST` and `SE` columns (for lavaan fits)
  that describe the test and standard error settings used to compute
  each row. When `standard_test = TRUE` adds the standard row, `SE` is
  set to `NA` for that row because fit indices do not depend on standard
  errors.

## Value

A data frame containing the fit indices for each model, with an
additional column identifying the models.

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
  fit1 <- cfa(hs_model, data = HolzingerSwineford1939, estimator = "ML")
  fit2 <- cfa(hs_model, data = HolzingerSwineford1939, estimator = "MLR")
  compare_model_fit(fit1, fit2)
  sem_model <- 'visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9
                textual ~ visual
                speed ~ textual'
  sem1 <- sem(sem_model, data = HolzingerSwineford1939, estimator = "ML")
  sem2 <- sem(sem_model, data = HolzingerSwineford1939, estimator = "MLR")
  compare_model_fit(sem1, sem2)
  compare_model_fit(fit1, fit2, standard_test = TRUE, test_details = TRUE)
  compare_model_fit(fit1, fit2, metrics = c("cfi", "tli", "rmsea"))
  fit3 <- cfa(
    hs_model,
    data = HolzingerSwineford1939,
    test = c("satorra.bentler", "mean.var.adjusted")
  )
  compare_model_fit(fit1, fit3, test = "satorra.bentler")
  compare_model_fit(
    fit1 = fit1,
    robust = fit3,
    test = list(robust = "mean.var.adjusted"),
    standard_test = list(robust = TRUE),
    test_details = TRUE
  )
} else {
  message("Please install 'lavaan' to run this example.")
}
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.
#> Standard-test row uses standard indices for estimator ML. Affected model: fit2.
#> cfi, tli, and rmsea were adjusted to their scaled version.
#> If you want to control the specific metric type used, specify it explicitly
#> (e.g., `cfi.robust`) or modify the type argument.
#> Requested tests not found in the fit1 model and were dropped: satorra.bentler.
#> Standard-test row uses standard indices for estimator ML. Affected model:
#> robust.
#> MODEL  | NOBS | ESTIMATOR  |       TEST        |    SE    | NPAR | Chi2 
#> ------------------------------------------------------------------------
#> fit1   | 301  |     ML     |     standard      | standard |  21  | 85.31
#> robust | 301  |     ML     |     standard      |          |  21  | 85.31
#> robust | 301  | ML_variant | mean.var.adjusted | standard |  21  | 67.23
#> 
#> MODEL  | Chi2_df | p (Chi2) |  CFI  |  TLI  | RMSEA |   RMSEA  CI    | SRMR 
#> ----------------------------------------------------------------------------
#> fit1   |  24.00  |  < .001  | 0.931 | 0.896 | 0.092 | [0.071, 0.114] | 0.065
#> robust |  24.00  |  < .001  | 0.931 | 0.896 | 0.092 | [0.071, 0.114] | 0.065
#> robust |  19.95  |  < .001  | 0.734 | 0.887 | 0.089 | [0.068, 0.110] | 0.065
#> 
```
