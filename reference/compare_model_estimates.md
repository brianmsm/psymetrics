# Compare Parameter Estimates Across Multiple Models

`compare_model_estimates()` compares parameter estimates from two or
more fitted models. It extracts estimates via
[`model_estimates()`](https://brianmsm.github.io/psymetrics/reference/model_estimates.md)
and aligns parameters by their structural identity so that shared and
model-specific parameters can be displayed side by side.

The current implementation is designed for fitted `lavaan` objects and
integrates with the package formatting and export workflow through
`prepare_table()`,
[`format_results()`](https://brianmsm.github.io/psymetrics/reference/format_results.md),
and
[`save_table()`](https://brianmsm.github.io/psymetrics/reference/save_table.md).

## Usage

``` r
compare_model_estimates(
  ...,
  standardized = FALSE,
  ci = 0.95,
  component = "all",
  select = "ci",
  verbose = TRUE
)
```

## Arguments

- ...:

  Two or more fitted model objects to compare.

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

- select:

  String indicating which statistics to show and how they should be
  arranged in the output table. Presets include `"ci"`, `"se"`,
  `"ci_p"`, `"se_p"`, `"ci_p2"`, and `"se_p2"`. Custom templates may use
  tokens like `{estimate}`, `{se}`, `{ci}`, `{ci_low}`, `{ci_high}`,
  `{p}`, and `{stars}`. This argument only controls visible statistics
  and layout; it does not filter parameter rows.

- verbose:

  Logical; if `TRUE`, prints informative messages emitted by the
  underlying extraction methods.

## Value

A data frame with class `compare_model_estimates`. The result contains
parameter identity columns (`Group`, `Level`, `Component`, `To`,
`Operator`, `From`) plus model-specific estimate columns such as
`Coefficient.<model>`, `SE.<model>`, `CI_low.<model>`,
`CI_high.<model>`, `z.<model>`, `p.<model>`, and `converged.<model>`.

## See also

[`model_estimates()`](https://brianmsm.github.io/psymetrics/reference/model_estimates.md)
for single-model extraction and
[`format_results()`](https://brianmsm.github.io/psymetrics/reference/format_results.md)
for output customization.

## Examples

``` r
if (requireNamespace("lavaan", quietly = TRUE)) {
  library(lavaan)
  library(psymetrics)

  hs_cfa_model <- '
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
  '

  hs_sem_model <- '
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
    textual ~ visual
    speed ~ visual + textual
  '

  fit1 <- cfa(hs_cfa_model, data = HolzingerSwineford1939)
  fit2 <- sem(hs_sem_model, data = HolzingerSwineford1939)

  compared <- compare_model_estimates(CFA = fit1, SEM = fit2, select = "se_p")
  compared
  format_results(compared, table_args = list(select = "{estimate} ({ci})|{p}"))
} else {
  message("Please install 'lavaan' to run this example.")
}
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.
#> 
#> 
#> Table: # Loading
#> 
#> |Link          |                  CFA | p (CFA) |                  SEM | p (SEM) |
#> |:-------------|:--------------------:|:-------:|:--------------------:|:-------:|
#> |visual =~ x1  | 1.000 (1.000, 1.000) |         | 1.000 (1.000, 1.000) |         |
#> |visual =~ x2  | 0.554 (0.358, 0.749) |  < .001 | 0.554 (0.358, 0.749) |  < .001 |
#> |visual =~ x3  | 0.729 (0.516, 0.943) |  < .001 | 0.729 (0.516, 0.943) |  < .001 |
#> |textual =~ x4 | 1.000 (1.000, 1.000) |         | 1.000 (1.000, 1.000) |         |
#> |textual =~ x5 | 1.113 (0.985, 1.241) |  < .001 | 1.113 (0.985, 1.241) |  < .001 |
#> |textual =~ x6 | 0.926 (0.817, 1.035) |  < .001 | 0.926 (0.817, 1.035) |  < .001 |
#> |speed =~ x7   | 1.000 (1.000, 1.000) |         | 1.000 (1.000, 1.000) |         |
#> |speed =~ x8   | 1.180 (0.857, 1.503) |  < .001 | 1.180 (0.857, 1.503) |  < .001 |
#> |speed =~ x9   | 1.082 (0.785, 1.378) |  < .001 | 1.082 (0.785, 1.378) |  < .001 |
#> 
#> Table: # Regression
#> 
#> |Link             | CFA | p (CFA) |                   SEM | p (SEM) |
#> |:----------------|:---:|:-------:|:---------------------:|:-------:|
#> |textual ~ visual |     |         |  0.504 (0.322, 0.687) |  < .001 |
#> |speed ~ visual   |     |         |  0.297 (0.144, 0.450) |  < .001 |
#> |speed ~ textual  |     |         | 0.053 (-0.051, 0.158) |   0.318 |
#> 
#> Table: # Correlation
#> 
#> |Link              |                  CFA | p (CFA) | SEM | p (SEM) |
#> |:-----------------|:--------------------:|:-------:|:---:|:-------:|
#> |visual ~~ textual | 0.408 (0.264, 0.552) |  < .001 |     |         |
#> |visual ~~ speed   | 0.262 (0.152, 0.373) |  < .001 |     |         |
#> |textual ~~ speed  | 0.173 (0.077, 0.270) |  < .001 |     |         |
#> 
#> Table: # Variance
#> 
#> |Link               |                  CFA | p (CFA) |                  SEM | p (SEM) |
#> |:------------------|:--------------------:|:-------:|:--------------------:|:-------:|
#> |x1 ~~ x1           | 0.549 (0.326, 0.772) |  < .001 | 0.549 (0.326, 0.772) |  < .001 |
#> |x2 ~~ x2           | 1.134 (0.934, 1.333) |  < .001 | 1.134 (0.934, 1.333) |  < .001 |
#> |x3 ~~ x3           | 0.844 (0.667, 1.022) |  < .001 | 0.844 (0.667, 1.022) |  < .001 |
#> |x4 ~~ x4           | 0.371 (0.278, 0.465) |  < .001 | 0.371 (0.278, 0.465) |  < .001 |
#> |x5 ~~ x5           | 0.446 (0.332, 0.561) |  < .001 | 0.446 (0.332, 0.561) |  < .001 |
#> |x6 ~~ x6           | 0.356 (0.272, 0.441) |  < .001 | 0.356 (0.272, 0.441) |  < .001 |
#> |x7 ~~ x7           | 0.799 (0.640, 0.959) |  < .001 | 0.799 (0.640, 0.959) |  < .001 |
#> |x8 ~~ x8           | 0.488 (0.342, 0.633) |  < .001 | 0.488 (0.342, 0.633) |  < .001 |
#> |x9 ~~ x9           | 0.566 (0.427, 0.705) |  < .001 | 0.566 (0.427, 0.705) |  < .001 |
#> |visual ~~ visual   | 0.809 (0.524, 1.094) |  < .001 | 0.809 (0.524, 1.094) |  < .001 |
#> |textual ~~ textual | 0.979 (0.760, 1.199) |  < .001 | 0.774 (0.582, 0.965) |  < .001 |
#> |speed ~~ speed     | 0.384 (0.215, 0.553) |  < .001 | 0.297 (0.160, 0.433) |  < .001 |
```
