# Get started with fit indices

## Goal

By the end of this guide, you will be able to:

1.  Fit a CFA model with `lavaan`.
2.  Extract fit indices with
    [`model_fit()`](https://brianmsm.github.io/psymetrics/reference/model_fit.md).
3.  Compare alternative estimators with
    [`compare_model_fit()`](https://brianmsm.github.io/psymetrics/reference/compare_model_fit.md).
4.  Fit an ordinal-item version with `WLSMV` and `ordered = TRUE`.

## Prerequisites

- R \>= 4.1.0
- Packages: `psymetrics`, `lavaan`

``` r
install.packages("lavaan")
```

## Setup

``` r
library(psymetrics)
library(lavaan)
```

## Step 1: Fit a baseline CFA model

``` r
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'

fit_mlr <- cfa(
  model,
  data = HolzingerSwineford1939,
  estimator = "MLR"
)
```

## Step 2: Extract fit indices

``` r
model_fit(fit_mlr)
#> NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> ---------------------------------------------------------------------
#> 301  |    MLR    |  21  |  87.13   |  < .001  | 0.925 | 0.888 | 0.093
#> 
#> NOBS |   RMSEA  CI    | SRMR 
#> -----------------------------
#> 301  | [0.073, 0.115] | 0.065
```

Request only selected metrics when needed:

``` r
model_fit(fit_mlr, metrics = c("cfi", "tli", "rmsea"))
#> NOBS | ESTIMATOR | NPAR |  CFI  |  TLI  | RMSEA
#> -----------------------------------------------
#> 301  |    MLR    |  21  | 0.925 | 0.888 | 0.093
```

## Step 3: Compare two fitted models

``` r
fit_ulsm <- cfa(
  model,
  data = HolzingerSwineford1939,
  estimator = "ULSM"
)
compare_model_fit(MLR = fit_mlr, ULSM = fit_ulsm)
#> MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> -----------------------------------------------------------------------------
#> MLR   | 301  |    MLR    |  21  |  87.13   |  < .001  | 0.925 | 0.888 | 0.093
#> ULSM  | 301  |   ULSM    |  21  |  90.60   |  < .001  | 0.931 | 0.897 | 0.096
#> 
#> MODEL |   RMSEA  CI    | SRMR 
#> ------------------------------
#> MLR   | [0.073, 0.115] | 0.065
#> ULSM  | [0.073, 0.120] | 0.059
```

## Step 4: Ordinal example with `WLSMV` (`ordered = TRUE`)

This example converts the nine Holzinger-Swineford items (`x1` to `x9`)
into four ordered categories, then fits the same CFA model with `WLSMV`.

``` r
item_names <- paste0("x", 1:9)
ordered_data <- HolzingerSwineford1939

ordered_data[item_names] <- lapply(
  ordered_data[item_names],
  function(x) ordered(cut(x, breaks = 4, include.lowest = TRUE))
)

fit_wlsmv <- cfa(
  model,
  data = ordered_data,
  estimator = "WLSMV",
  ordered = TRUE
)

model_fit(fit_wlsmv)
#> NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> ---------------------------------------------------------------------
#> 301  |   WLSMV   |  39  |  63.89   |  < .001  | 0.969 | 0.954 | 0.074
#> 
#> NOBS |   RMSEA  CI    | SRMR 
#> -----------------------------
#> 301  | [0.053, 0.097] | 0.066
compare_model_fit(MLR = fit_mlr, WLSMV = fit_wlsmv)
#> MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> -----------------------------------------------------------------------------
#> MLR   | 301  |    MLR    |  21  |  87.13   |  < .001  | 0.925 | 0.888 | 0.093
#> WLSMV | 301  |   WLSMV   |  39  |  63.89   |  < .001  | 0.969 | 0.954 | 0.074
#> 
#> MODEL |   RMSEA  CI    | SRMR 
#> ------------------------------
#> MLR   | [0.073, 0.115] | 0.065
#> WLSMV | [0.053, 0.097] | 0.066
```

## Common pitfalls

- When using robust estimators, metric names may map to scaled/robust
  variants.
- Keep model names explicit (`MLR`, `ULSM`, etc.) for clearer comparison
  tables.
- This ordinal recoding is only a didactic example; use substantively
  justified category definitions in real analyses.

## Next steps

- Continue with [Reporting and
  visualization](https://brianmsm.github.io/psymetrics/articles/reporting-and-visualization.md)
  to turn fit outputs into tables and plots.
- Continue with [SEM and parameter
  estimates](https://brianmsm.github.io/psymetrics/articles/sem-and-estimates-lavaan.md).
- See full API details in the
  [Reference](https://brianmsm.github.io/psymetrics/reference/index.md).
