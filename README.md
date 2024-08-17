
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psymetrics

<!-- badges: start -->
<!-- badges: end -->

The goal of psymetrics is to provide tools for extracting and
visualizing psychometric model fit indices. It is compatible with models
created using packages like lavaan, psych, and mirt.

## Installation

You can install the development version of psymetrics from
[GitHub](https://github.com/brianmsm/psymetrics) with:

``` r
# install.packages("pak")
pak::pak("brianmsm/psymetrics")
```

## Example

Here is an example of how to use the psymetrics package with a model
created using lavaan.

``` r
library(psymetrics)
library(lavaan)
#> This is lavaan 0.6-18
#> lavaan is FREE software! Please report any bugs.

# Define a simple CFA model
model <- 'visual  =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed   =~ x7 + x8 + x9'

# Fit the model using lavaan
fit <- cfa(model, data = HolzingerSwineford1939, estimator = "MLR")

# Extract and print fit indices
model_fit(fit)
#> NOBS    | ESTIMATOR |   NPAR | Chi2(24) | p (Chi2) |   CFI |   TLI | RMSEA |    RMSEA  CI |  SRMR
#> -------------------------------------------------------------------------------------------------
#> 301.000 |       MLR | 21.000 |   87.132 |   < .001 | 0.925 | 0.888 | 0.093 | [0.07, 0.12] | 0.065

# You can also request specific types of indices, such as 'robust'
model_fit(fit, type = "robust")
#> NOBS    | ESTIMATOR |   NPAR | Chi2(24) | p (Chi2) |   CFI |   TLI | RMSEA |    RMSEA  CI |  SRMR
#> -------------------------------------------------------------------------------------------------
#> 301.000 |       MLR | 21.000 |   87.132 |   < .001 | 0.930 | 0.895 | 0.092 | [0.07, 0.11] | 0.065

# Or specify which indices to extract
model_fit(fit, metrics = c("cfi", "tli"))
#> cfi and tli were adjusted to their scaled version.
#> If you want to control the specific metric type used, specify it explicitly
#> (e.g., `cfi.robust`) or modify the type argument.
#> NOBS    | ESTIMATOR |   NPAR |   CFI |   TLI
#> --------------------------------------------
#> 301.000 |       MLR | 21.000 | 0.925 | 0.888
```

This example demonstrates how to extract and print various fit indices
from a confirmatory factor analysis (CFA) model using psymetrics. You
can choose between standard, scaled, or robust fit indices, and even
specify custom sets of indices to extract.
