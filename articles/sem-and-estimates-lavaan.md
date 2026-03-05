# SEM and parameter estimates with lavaan

## Goal

This article shows how to:

1.  Fit a SEM model with
    [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).
2.  Compare fit across estimators.
3.  Extract parameter estimates by component.

## Prerequisites

- Packages: `psymetrics`, `lavaan`

## Setup

``` r
library(psymetrics)
library(lavaan)
```

## Fit SEM models

``` r
sem_model <- '
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8

  dem60 ~ ind60
  dem65 ~ ind60 + dem60
'

fit_sem_mlr <- sem(
  sem_model,
  data = PoliticalDemocracy,
  estimator = "MLR"
)
fit_sem_ml <- sem(
  sem_model,
  data = PoliticalDemocracy,
  estimator = "ML"
)
```

## Inspect and compare fit

``` r
model_fit(fit_sem_mlr)
#> NOBS | ESTIMATOR | NPAR | Chi2(41) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> ---------------------------------------------------------------------
#> 75   |    MLR    |  25  |  73.78   |  0.001   | 0.949 | 0.932 | 0.103
#> 
#> NOBS |   RMSEA  CI    | SRMR 
#> -----------------------------
#> 75   | [0.064, 0.141] | 0.055
compare_model_fit(MLR = fit_sem_mlr, ML = fit_sem_ml)
#> MODEL | NOBS | ESTIMATOR | NPAR | Chi2(41) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> -----------------------------------------------------------------------------
#> MLR   |  75  |    MLR    |  25  |  73.78   |  0.001   | 0.949 | 0.932 | 0.103
#> ML    |  75  |    ML     |  25  |  72.46   |  0.002   | 0.953 | 0.938 | 0.101
#> 
#> MODEL |   RMSEA  CI    | SRMR 
#> ------------------------------
#> MLR   | [0.064, 0.141] | 0.055
#> ML    | [0.061, 0.139] | 0.055
```

## Extract parameter estimates

All parameter blocks:

``` r
model_estimates(fit_sem_mlr)
```

Only selected components:

``` r
model_estimates(
  fit_sem_mlr,
  component = c("loading", "regression"),
  standardized = TRUE
)
```

## Practical notes

- Use `component` to narrow output to the section relevant for your
  report.
- Use `standardized = TRUE` when comparing relative magnitudes.

## Next steps

- Continue with [Reporting and
  visualization](https://brianmsm.github.io/psymetrics/articles/reporting-and-visualization.md).
- See function-level details in the
  [Reference](https://brianmsm.github.io/psymetrics/reference/index.md).
