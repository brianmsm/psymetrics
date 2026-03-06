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
#> # Loading
#> 
#> Link        | Coefficient |  SE   |       CI       |   z    |   p   
#> --------------------------------------------------------------------
#> ind60 =~ x1 |    1.000    | 0.000 | [1.000, 1.000] |        |       
#> ind60 =~ x2 |    2.182    | 0.144 | [1.899, 2.464] | 15.133 | < .001
#> ind60 =~ x3 |    1.819    | 0.140 | [1.544, 2.093] | 12.979 | < .001
#> dem60 =~ y1 |    1.000    | 0.000 | [1.000, 1.000] |        |       
#> dem60 =~ y2 |    1.354    | 0.158 | [1.045, 1.663] | 8.596  | < .001
#> dem60 =~ y3 |    1.044    | 0.127 | [0.796, 1.292] | 8.246  | < .001
#> dem60 =~ y4 |    1.300    | 0.132 | [1.041, 1.558] | 9.853  | < .001
#> dem65 =~ y5 |    1.000    | 0.000 | [1.000, 1.000] |        |       
#> dem65 =~ y6 |    1.258    | 0.198 | [0.870, 1.647] | 6.344  | < .001
#> dem65 =~ y7 |    1.282    | 0.160 | [0.969, 1.596] | 8.015  | < .001
#> dem65 =~ y8 |    1.310    | 0.186 | [0.946, 1.673] | 7.059  | < .001
#> 
#> # Regression
#> 
#> Link          | Coefficient |  SE   |       CI       |   z    |   p   
#> ----------------------------------------------------------------------
#> dem60 ~ ind60 |    1.474    | 0.331 | [0.824, 2.123] | 4.449  | < .001
#> dem65 ~ ind60 |    0.453    | 0.213 | [0.035, 0.872] | 2.123  | 0.034 
#> dem65 ~ dem60 |    0.864    | 0.086 | [0.696, 1.033] | 10.032 | < .001
#> 
#> # Variance
#> 
#> Link           | Coefficient |  SE   |       CI        |   z   |   p   
#> -----------------------------------------------------------------------
#> x1 ~~ x1       |    0.082    | 0.018 | [ 0.046, 0.118] | 4.427 | < .001
#> x2 ~~ x2       |    0.118    | 0.072 | [-0.024, 0.260] | 1.635 | 0.102 
#> x3 ~~ x3       |    0.467    | 0.083 | [ 0.304, 0.630] | 5.624 | < .001
#> y1 ~~ y1       |    1.942    | 0.400 | [ 1.158, 2.725] | 4.858 | < .001
#> y2 ~~ y2       |    6.490    | 1.353 | [ 3.838, 9.141] | 4.798 | < .001
#> y3 ~~ y3       |    5.340    | 1.091 | [ 3.201, 7.479] | 4.893 | < .001
#> y4 ~~ y4       |    2.887    | 0.626 | [ 1.660, 4.115] | 4.610 | < .001
#> y5 ~~ y5       |    2.390    | 0.566 | [ 1.280, 3.500] | 4.220 | < .001
#> y6 ~~ y6       |    4.343    | 0.865 | [ 2.647, 6.039] | 5.020 | < .001
#> y7 ~~ y7       |    3.510    | 0.586 | [ 2.361, 4.658] | 5.990 | < .001
#> y8 ~~ y8       |    2.940    | 0.803 | [ 1.367, 4.514] | 3.663 | < .001
#> ind60 ~~ ind60 |    0.448    | 0.073 | [ 0.305, 0.591] | 6.148 | < .001
#> dem60 ~~ dem60 |    3.872    | 0.836 | [ 2.232, 5.511] | 4.629 | < .001
#> dem65 ~~ dem65 |    0.115    | 0.215 | [-0.307, 0.537] | 0.534 | 0.594
```

Only selected components:

``` r
model_estimates(
  fit_sem_mlr,
  component = c("loading", "regression"),
  standardized = TRUE
)
#> # Loading
#> 
#> Link        | Coefficient |  SE   |       CI       |   z    |   p   
#> --------------------------------------------------------------------
#> ind60 =~ x1 |    0.920    | 0.022 | [0.876, 0.963] | 41.336 | < .001
#> ind60 =~ x2 |    0.973    | 0.016 | [0.941, 1.005] | 59.590 | < .001
#> ind60 =~ x3 |    0.872    | 0.029 | [0.815, 0.929] | 30.097 | < .001
#> dem60 =~ y1 |    0.845    | 0.039 | [0.768, 0.922] | 21.555 | < .001
#> dem60 =~ y2 |    0.760    | 0.057 | [0.648, 0.872] | 13.329 | < .001
#> dem60 =~ y3 |    0.705    | 0.071 | [0.566, 0.844] | 9.924  | < .001
#> dem60 =~ y4 |    0.860    | 0.037 | [0.787, 0.933] | 23.020 | < .001
#> dem65 =~ y5 |    0.803    | 0.057 | [0.692, 0.914] | 14.185 | < .001
#> dem65 =~ y6 |    0.783    | 0.052 | [0.682, 0.884] | 15.139 | < .001
#> dem65 =~ y7 |    0.819    | 0.035 | [0.750, 0.888] | 23.359 | < .001
#> dem65 =~ y8 |    0.847    | 0.048 | [0.752, 0.942] | 17.499 | < .001
#> 
#> # Regression
#> 
#> Link          | Coefficient |  SE   |       CI       |   z    |   p   
#> ----------------------------------------------------------------------
#> dem60 ~ ind60 |    0.448    | 0.107 | [0.238, 0.658] | 4.179  | < .001
#> dem65 ~ ind60 |    0.146    | 0.064 | [0.020, 0.271] | 2.267  | 0.023 
#> dem65 ~ dem60 |    0.913    | 0.046 | [0.822, 1.004] | 19.672 | < .001
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
