# Extract Parameter Estimates from a lavaan Model

`model_estimates.lavaan()` extracts CFA/SEM parameter estimates from a
fitted `lavaan` object and organizes them into components (for example,
loadings, regressions, variances, and thresholds). The output is
designed to integrate with `psymetrics` formatting and export methods.

## Usage

``` r
# S3 method for class 'lavaan'
model_estimates(
  fit,
  standardized = FALSE,
  ci = 0.95,
  component = "all",
  verbose = TRUE,
  ...
)
```

## Arguments

- fit:

  A fitted `lavaan` object (for example from
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html),
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html), or
  [`lavaan::growth()`](https://rdrr.io/pkg/lavaan/man/growth.html)).

- standardized:

  Controls standardized extraction. Accepted values are `FALSE`, `TRUE`,
  `"std.all"`, `"std.lv"`, `"std.nox"`, plus aliases: `"all"`,
  `"latent"`, `"lv"`, and `"no_exogenous"`.

- ci:

  Confidence level for interval columns. Must be a numeric value in
  `(0, 1)`.

- component:

  Character vector indicating which parameter components to keep. Use
  `"all"` (default) to keep all available components.

- verbose:

  Logical; if `TRUE`, prints informative messages about non-convergence
  and partially unavailable inferential statistics.

- ...:

  Additional arguments passed to methods.

## Value

A data frame with class `model_estimates`. Core columns are: `To`,
`Operator`, `From`, `Coefficient`, `SE`, `CI_low`, `CI_high`, `z`, `p`,
`Component`, and `converged`.

## See also

[`model_estimates()`](https://brianmsm.github.io/psymetrics/reference/model_estimates.md)
for the generic.

## Examples

``` r
if (requireNamespace("lavaan", quietly = TRUE)) {
  library(lavaan)
  library(psymetrics)

  sem_model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  '

  fit <- sem(sem_model, data = PoliticalDemocracy)
  model_estimates(fit)
  model_estimates(fit, component = c("loading", "regression"))
  model_estimates(fit, standardized = "std.lv")
} else {
  message("Please install 'lavaan' to run this example.")
}
#> # Loading
#> 
#> Link        | Coefficient |  SE   |       CI       |   z    |   p   
#> --------------------------------------------------------------------
#> ind60 =~ x1 |    0.669    | 0.065 | [0.543, 0.796] | 10.339 | < .001
#> ind60 =~ x2 |    1.461    | 0.128 | [1.210, 1.711] | 11.428 | < .001
#> ind60 =~ x3 |    1.218    | 0.128 | [0.966, 1.469] | 9.477  | < .001
#> dem60 =~ y1 |    2.201    | 0.247 | [1.717, 2.685] | 8.907  | < .001
#> dem60 =~ y2 |    2.980    | 0.392 | [2.212, 3.749] | 7.604  | < .001
#> dem60 =~ y3 |    2.298    | 0.335 | [1.641, 2.955] | 6.851  | < .001
#> dem60 =~ y4 |    2.860    | 0.312 | [2.248, 3.473] | 9.155  | < .001
#> dem65 =~ y5 |    2.084    | 0.252 | [1.590, 2.578] | 8.269  | < .001
#> dem65 =~ y6 |    2.623    | 0.329 | [1.977, 3.269] | 7.963  | < .001
#> dem65 =~ y7 |    2.673    | 0.314 | [2.058, 3.288] | 8.515  | < .001
#> dem65 =~ y8 |    2.730    | 0.304 | [2.133, 3.327] | 8.968  | < .001
#> 
#> # Regression
#> 
#> Link          | Coefficient |  SE   |       CI       |   z    |   p   
#> ----------------------------------------------------------------------
#> dem60 ~ ind60 |    0.448    | 0.102 | [0.248, 0.648] | 4.393  | < .001
#> dem65 ~ ind60 |    0.146    | 0.070 | [0.008, 0.283] | 2.071  | 0.038 
#> dem65 ~ dem60 |    0.913    | 0.048 | [0.819, 1.006] | 19.120 | < .001
#> 
#> # Variance
#> 
#> Link           | Coefficient |  SE   |       CI        |   z   |   p   
#> -----------------------------------------------------------------------
#> x1 ~~ x1       |    0.082    | 0.020 | [ 0.043, 0.120] | 4.180 | < .001
#> x2 ~~ x2       |    0.118    | 0.070 | [-0.019, 0.256] | 1.689 | 0.091 
#> x3 ~~ x3       |    0.467    | 0.090 | [ 0.290, 0.644] | 5.174 | < .001
#> y1 ~~ y1       |    1.942    | 0.395 | [ 1.167, 2.717] | 4.910 | < .001
#> y2 ~~ y2       |    6.490    | 1.185 | [ 4.168, 8.811] | 5.479 | < .001
#> y3 ~~ y3       |    5.340    | 0.943 | [ 3.491, 7.188] | 5.662 | < .001
#> y4 ~~ y4       |    2.887    | 0.610 | [ 1.691, 4.083] | 4.731 | < .001
#> y5 ~~ y5       |    2.390    | 0.447 | [ 1.515, 3.265] | 5.351 | < .001
#> y6 ~~ y6       |    4.343    | 0.796 | [ 2.783, 5.903] | 5.456 | < .001
#> y7 ~~ y7       |    3.510    | 0.668 | [ 2.200, 4.819] | 5.252 | < .001
#> y8 ~~ y8       |    2.940    | 0.586 | [ 1.792, 4.089] | 5.019 | < .001
#> ind60 ~~ ind60 |    1.000    | 0.000 | [ 1.000, 1.000] |       |       
#> dem60 ~~ dem60 |    0.799    | 0.091 | [ 0.620, 0.978] | 8.737 | < .001
#> dem65 ~~ dem65 |    0.026    | 0.046 | [-0.063, 0.116] | 0.579 | 0.562 
#> 
```
