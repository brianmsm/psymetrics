# SEM, parameter estimates, and comparisons with lavaan

## Goal

This article shows how to:

1.  Fit related CFA and SEM models with `lavaan`.
2.  Inspect and compare model fit.
3.  Extract parameter estimates by component with
    [`model_estimates()`](https://brianmsm.github.io/psymetrics/reference/model_estimates.md).
4.  Compare parameter estimates across models with
    [`compare_model_estimates()`](https://brianmsm.github.io/psymetrics/reference/compare_model_estimates.md).
5.  Customize comparison layouts with public `select` presets and
    advanced `table_args` overrides.

## Prerequisites

- Packages: `psymetrics`, `lavaan`

## Setup

``` r
library(psymetrics)
library(lavaan)
```

## Fit related CFA and SEM models

We will use the same measurement structure in two fitted models:

- a CFA model with three latent factors;
- a SEM model that keeps the same measurement model and adds regressions
  among factors.

This gives us a realistic comparison where some parameters are shared
across models and some appear only in the SEM.

``` r
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

fit_cfa <- cfa(hs_cfa_model, data = HolzingerSwineford1939)
fit_sem <- sem(hs_sem_model, data = HolzingerSwineford1939)
```

## Inspect and compare fit

You can still use the fit-oriented helpers before moving to
parameter-level output.

``` r
model_fit(fit_cfa)
#> NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> ---------------------------------------------------------------------
#> 301  |    ML     |  21  |  85.31   |  < .001  | 0.931 | 0.896 | 0.092
#> 
#> NOBS |   RMSEA  CI    | SRMR 
#> -----------------------------
#> 301  | [0.071, 0.114] | 0.065
compare_model_fit(CFA = fit_cfa, SEM = fit_sem)
#> MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> -----------------------------------------------------------------------------
#> CFA   | 301  |    ML     |  21  |  85.31   |  < .001  | 0.931 | 0.896 | 0.092
#> SEM   | 301  |    ML     |  21  |  85.31   |  < .001  | 0.931 | 0.896 | 0.092
#> 
#> MODEL |   RMSEA  CI    | SRMR 
#> ------------------------------
#> CFA   | [0.071, 0.114] | 0.065
#> SEM   | [0.071, 0.114] | 0.065
```

## Extract parameter estimates with `model_estimates()`

For a single fitted model,
[`model_estimates()`](https://brianmsm.github.io/psymetrics/reference/model_estimates.md)
gives you parameter blocks such as loadings, regressions, variances, and
means.

``` r
model_estimates(fit_sem)
#> # Loading
#> 
#> Link          | Coefficient |  SE   |       CI       |   z    |   p   
#> ----------------------------------------------------------------------
#> visual =~ x1  |    1.000    | 0.000 | [1.000, 1.000] |        |       
#> visual =~ x2  |    0.554    | 0.100 | [0.358, 0.749] | 5.554  | < .001
#> visual =~ x3  |    0.729    | 0.109 | [0.516, 0.943] | 6.685  | < .001
#> textual =~ x4 |    1.000    | 0.000 | [1.000, 1.000] |        |       
#> textual =~ x5 |    1.113    | 0.065 | [0.985, 1.241] | 17.014 | < .001
#> textual =~ x6 |    0.926    | 0.055 | [0.817, 1.035] | 16.703 | < .001
#> speed =~ x7   |    1.000    | 0.000 | [1.000, 1.000] |        |       
#> speed =~ x8   |    1.180    | 0.165 | [0.857, 1.503] | 7.152  | < .001
#> speed =~ x9   |    1.082    | 0.151 | [0.785, 1.378] | 7.155  | < .001
#> 
#> # Regression
#> 
#> Link             | Coefficient |  SE   |       CI        |   z   |   p   
#> -------------------------------------------------------------------------
#> textual ~ visual |    0.504    | 0.093 | [ 0.322, 0.687] | 5.420 | < .001
#> speed ~ visual   |    0.297    | 0.078 | [ 0.144, 0.450] | 3.801 | < .001
#> speed ~ textual  |    0.053    | 0.053 | [-0.051, 0.158] | 0.999 | 0.318 
#> 
#> # Variance
#> 
#> Link               | Coefficient |  SE   |       CI       |   z    |   p   
#> ---------------------------------------------------------------------------
#> x1 ~~ x1           |    0.549    | 0.114 | [0.326, 0.772] | 4.833  | < .001
#> x2 ~~ x2           |    1.134    | 0.102 | [0.934, 1.333] | 11.146 | < .001
#> x3 ~~ x3           |    0.844    | 0.091 | [0.667, 1.022] | 9.317  | < .001
#> x4 ~~ x4           |    0.371    | 0.048 | [0.278, 0.465] | 7.779  | < .001
#> x5 ~~ x5           |    0.446    | 0.058 | [0.332, 0.561] | 7.642  | < .001
#> x6 ~~ x6           |    0.356    | 0.043 | [0.272, 0.441] | 8.277  | < .001
#> x7 ~~ x7           |    0.799    | 0.081 | [0.640, 0.959] | 9.823  | < .001
#> x8 ~~ x8           |    0.488    | 0.074 | [0.342, 0.633] | 6.573  | < .001
#> x9 ~~ x9           |    0.566    | 0.071 | [0.427, 0.705] | 8.003  | < .001
#> visual ~~ visual   |    0.809    | 0.145 | [0.524, 1.094] | 5.564  | < .001
#> textual ~~ textual |    0.774    | 0.097 | [0.582, 0.965] | 7.935  | < .001
#> speed ~~ speed     |    0.297    | 0.070 | [0.160, 0.433] | 4.253  | < .001
```

You can narrow the output to the parts most relevant to your workflow.
For example, when you only want loadings and regressions:

``` r
model_estimates(
  fit_sem,
  component = c("loading", "regression"),
  standardized = TRUE
)
#> # Loading
#> 
#> Link          | Coefficient |  SE   |       CI       |   z    |   p   
#> ----------------------------------------------------------------------
#> visual =~ x1  |    0.772    | 0.055 | [0.664, 0.880] | 14.041 | < .001
#> visual =~ x2  |    0.424    | 0.060 | [0.307, 0.540] | 7.105  | < .001
#> visual =~ x3  |    0.581    | 0.055 | [0.473, 0.689] | 10.539 | < .001
#> textual =~ x4 |    0.852    | 0.023 | [0.807, 0.896] | 37.776 | < .001
#> textual =~ x5 |    0.855    | 0.022 | [0.811, 0.899] | 38.273 | < .001
#> textual =~ x6 |    0.838    | 0.023 | [0.792, 0.884] | 35.881 | < .001
#> speed =~ x7   |    0.570    | 0.053 | [0.465, 0.674] | 10.714 | < .001
#> speed =~ x8   |    0.723    | 0.051 | [0.624, 0.822] | 14.309 | < .001
#> speed =~ x9   |    0.665    | 0.051 | [0.565, 0.765] | 13.015 | < .001
#> 
#> # Regression
#> 
#> Link             | Coefficient |  SE   |       CI        |   z   |   p   
#> -------------------------------------------------------------------------
#> textual ~ visual |    0.459    | 0.064 | [ 0.334, 0.584] | 7.189 | < .001
#> speed ~ visual   |    0.431    | 0.090 | [ 0.255, 0.608] | 4.802 | < .001
#> speed ~ textual  |    0.085    | 0.085 | [-0.081, 0.251] | 1.007 | 0.314
```

## Compare estimates across models

The simplest comparison uses the default `select = "ci"`, which shows
estimates with confidence intervals.

``` r
compared_default <- compare_model_estimates(CFA = fit_cfa, SEM = fit_sem)
compared_default
#> # Loading
#> 
#> Link          |         CFA          |         SEM         
#> -----------------------------------------------------------
#> visual =~ x1  | 1.000 (1.000, 1.000) | 1.000 (1.000, 1.000)
#> visual =~ x2  | 0.554 (0.358, 0.749) | 0.554 (0.358, 0.749)
#> visual =~ x3  | 0.729 (0.516, 0.943) | 0.729 (0.516, 0.943)
#> textual =~ x4 | 1.000 (1.000, 1.000) | 1.000 (1.000, 1.000)
#> textual =~ x5 | 1.113 (0.985, 1.241) | 1.113 (0.985, 1.241)
#> textual =~ x6 | 0.926 (0.817, 1.035) | 0.926 (0.817, 1.035)
#> speed =~ x7   | 1.000 (1.000, 1.000) | 1.000 (1.000, 1.000)
#> speed =~ x8   | 1.180 (0.857, 1.503) | 1.180 (0.857, 1.503)
#> speed =~ x9   | 1.082 (0.785, 1.378) | 1.082 (0.785, 1.378)
#> 
#> # Regression
#> 
#> Link             | CFA |          SEM         
#> ----------------------------------------------
#> textual ~ visual |     | 0.504 (0.322, 0.687) 
#> speed ~ visual   |     | 0.297 (0.144, 0.450) 
#> speed ~ textual  |     | 0.053 (-0.051, 0.158)
#> 
#> # Correlation
#> 
#> Link              |         CFA          | SEM
#> ----------------------------------------------
#> visual ~~ textual | 0.408 (0.264, 0.552) |    
#> visual ~~ speed   | 0.262 (0.152, 0.373) |    
#> textual ~~ speed  | 0.173 (0.077, 0.270) |    
#> 
#> # Variance
#> 
#> Link               |         CFA          |         SEM         
#> ----------------------------------------------------------------
#> x1 ~~ x1           | 0.549 (0.326, 0.772) | 0.549 (0.326, 0.772)
#> x2 ~~ x2           | 1.134 (0.934, 1.333) | 1.134 (0.934, 1.333)
#> x3 ~~ x3           | 0.844 (0.667, 1.022) | 0.844 (0.667, 1.022)
#> x4 ~~ x4           | 0.371 (0.278, 0.465) | 0.371 (0.278, 0.465)
#> x5 ~~ x5           | 0.446 (0.332, 0.561) | 0.446 (0.332, 0.561)
#> x6 ~~ x6           | 0.356 (0.272, 0.441) | 0.356 (0.272, 0.441)
#> x7 ~~ x7           | 0.799 (0.640, 0.959) | 0.799 (0.640, 0.959)
#> x8 ~~ x8           | 0.488 (0.342, 0.633) | 0.488 (0.342, 0.633)
#> x9 ~~ x9           | 0.566 (0.427, 0.705) | 0.566 (0.427, 0.705)
#> visual ~~ visual   | 0.809 (0.524, 1.094) | 0.809 (0.524, 1.094)
#> textual ~~ textual | 0.979 (0.760, 1.199) | 0.774 (0.582, 0.965)
#> speed ~~ speed     | 0.384 (0.215, 0.553) | 0.297 (0.160, 0.433)
```

This is especially useful when one model contains parameters that do not
exist in the other. In this example, the regression rows only appear for
the SEM, while shared loadings still line up across both models.

## Focus the comparison on selected components

As with
[`model_estimates()`](https://brianmsm.github.io/psymetrics/reference/model_estimates.md),
you can restrict the comparison to a subset of components.

``` r
compare_model_estimates(
  CFA = fit_cfa,
  SEM = fit_sem,
  component = c("loading", "regression")
)
#> # Loading
#> 
#> Link          |         CFA          |         SEM         
#> -----------------------------------------------------------
#> visual =~ x1  | 1.000 (1.000, 1.000) | 1.000 (1.000, 1.000)
#> visual =~ x2  | 0.554 (0.358, 0.749) | 0.554 (0.358, 0.749)
#> visual =~ x3  | 0.729 (0.516, 0.943) | 0.729 (0.516, 0.943)
#> textual =~ x4 | 1.000 (1.000, 1.000) | 1.000 (1.000, 1.000)
#> textual =~ x5 | 1.113 (0.985, 1.241) | 1.113 (0.985, 1.241)
#> textual =~ x6 | 0.926 (0.817, 1.035) | 0.926 (0.817, 1.035)
#> speed =~ x7   | 1.000 (1.000, 1.000) | 1.000 (1.000, 1.000)
#> speed =~ x8   | 1.180 (0.857, 1.503) | 1.180 (0.857, 1.503)
#> speed =~ x9   | 1.082 (0.785, 1.378) | 1.082 (0.785, 1.378)
#> 
#> # Regression
#> 
#> Link             | CFA |          SEM         
#> ----------------------------------------------
#> textual ~ visual |     | 0.504 (0.322, 0.687) 
#> speed ~ visual   |     | 0.297 (0.144, 0.450) 
#> speed ~ textual  |     | 0.053 (-0.051, 0.158)
```

Use this when you want a tighter table for a manuscript, report, or
diagnostics pass.

## Use public `select` presets

The public `select` argument controls which statistics are shown and how
they are arranged by default for the object.

For example, `select = "se_p"` shows estimates, standard errors, and
significance stars in a compact single column per model.

``` r
compared_se <- compare_model_estimates(
  CFA = fit_cfa,
  SEM = fit_sem,
  select = "se_p"
)
compared_se
#> # Loading
#> 
#> Link          |       CFA        |       SEM       
#> ---------------------------------------------------
#> visual =~ x1  |  1.000 (0.000)   |  1.000 (0.000)  
#> visual =~ x2  | 0.554*** (0.100) | 0.554*** (0.100)
#> visual =~ x3  | 0.729*** (0.109) | 0.729*** (0.109)
#> textual =~ x4 |  1.000 (0.000)   |  1.000 (0.000)  
#> textual =~ x5 | 1.113*** (0.065) | 1.113*** (0.065)
#> textual =~ x6 | 0.926*** (0.055) | 0.926*** (0.055)
#> speed =~ x7   |  1.000 (0.000)   |  1.000 (0.000)  
#> speed =~ x8   | 1.180*** (0.165) | 1.180*** (0.165)
#> speed =~ x9   | 1.082*** (0.151) | 1.082*** (0.151)
#> 
#> # Regression
#> 
#> Link             | CFA |       SEM       
#> -----------------------------------------
#> textual ~ visual |     | 0.504*** (0.093)
#> speed ~ visual   |     | 0.297*** (0.078)
#> speed ~ textual  |     |  0.053 (0.053)  
#> 
#> # Correlation
#> 
#> Link              |       CFA        | SEM
#> ------------------------------------------
#> visual ~~ textual | 0.408*** (0.074) |    
#> visual ~~ speed   | 0.262*** (0.056) |    
#> textual ~~ speed  | 0.173*** (0.049) |    
#> 
#> # Variance
#> 
#> Link               |       CFA        |       SEM       
#> --------------------------------------------------------
#> x1 ~~ x1           | 0.549*** (0.114) | 0.549*** (0.114)
#> x2 ~~ x2           | 1.134*** (0.102) | 1.134*** (0.102)
#> x3 ~~ x3           | 0.844*** (0.091) | 0.844*** (0.091)
#> x4 ~~ x4           | 0.371*** (0.048) | 0.371*** (0.048)
#> x5 ~~ x5           | 0.446*** (0.058) | 0.446*** (0.058)
#> x6 ~~ x6           | 0.356*** (0.043) | 0.356*** (0.043)
#> x7 ~~ x7           | 0.799*** (0.081) | 0.799*** (0.081)
#> x8 ~~ x8           | 0.488*** (0.074) | 0.488*** (0.074)
#> x9 ~~ x9           | 0.566*** (0.071) | 0.566*** (0.071)
#> visual ~~ visual   | 0.809*** (0.145) | 0.809*** (0.145)
#> textual ~~ textual | 0.979*** (0.112) | 0.774*** (0.097)
#> speed ~~ speed     | 0.384*** (0.086) | 0.297*** (0.070)
```

If you prefer numeric p-values in a second column, use a two-column
preset such as `"ci_p2"`.

``` r
compare_model_estimates(
  CFA = fit_cfa,
  SEM = fit_sem,
  select = "ci_p2"
)
#> # Loading
#> 
#> Link          |         CFA          | p (CFA) |         SEM          | p (SEM)
#> -------------------------------------------------------------------------------
#> visual =~ x1  | 1.000 (1.000, 1.000) |         | 1.000 (1.000, 1.000) |        
#> visual =~ x2  | 0.554 (0.358, 0.749) | < .001  | 0.554 (0.358, 0.749) | < .001 
#> visual =~ x3  | 0.729 (0.516, 0.943) | < .001  | 0.729 (0.516, 0.943) | < .001 
#> textual =~ x4 | 1.000 (1.000, 1.000) |         | 1.000 (1.000, 1.000) |        
#> textual =~ x5 | 1.113 (0.985, 1.241) | < .001  | 1.113 (0.985, 1.241) | < .001 
#> textual =~ x6 | 0.926 (0.817, 1.035) | < .001  | 0.926 (0.817, 1.035) | < .001 
#> speed =~ x7   | 1.000 (1.000, 1.000) |         | 1.000 (1.000, 1.000) |        
#> speed =~ x8   | 1.180 (0.857, 1.503) | < .001  | 1.180 (0.857, 1.503) | < .001 
#> speed =~ x9   | 1.082 (0.785, 1.378) | < .001  | 1.082 (0.785, 1.378) | < .001 
#> 
#> # Regression
#> 
#> Link             | CFA | p (CFA) |          SEM          | p (SEM)
#> ------------------------------------------------------------------
#> textual ~ visual |     |         | 0.504 (0.322, 0.687)  | < .001 
#> speed ~ visual   |     |         | 0.297 (0.144, 0.450)  | < .001 
#> speed ~ textual  |     |         | 0.053 (-0.051, 0.158) |  0.318 
#> 
#> # Correlation
#> 
#> Link              |         CFA          | p (CFA) | SEM | p (SEM)
#> ------------------------------------------------------------------
#> visual ~~ textual | 0.408 (0.264, 0.552) | < .001  |     |        
#> visual ~~ speed   | 0.262 (0.152, 0.373) | < .001  |     |        
#> textual ~~ speed  | 0.173 (0.077, 0.270) | < .001  |     |        
#> 
#> # Variance
#> 
#> Link               |         CFA          | p (CFA) |         SEM          | p (SEM)
#> ------------------------------------------------------------------------------------
#> x1 ~~ x1           | 0.549 (0.326, 0.772) | < .001  | 0.549 (0.326, 0.772) | < .001 
#> x2 ~~ x2           | 1.134 (0.934, 1.333) | < .001  | 1.134 (0.934, 1.333) | < .001 
#> x3 ~~ x3           | 0.844 (0.667, 1.022) | < .001  | 0.844 (0.667, 1.022) | < .001 
#> x4 ~~ x4           | 0.371 (0.278, 0.465) | < .001  | 0.371 (0.278, 0.465) | < .001 
#> x5 ~~ x5           | 0.446 (0.332, 0.561) | < .001  | 0.446 (0.332, 0.561) | < .001 
#> x6 ~~ x6           | 0.356 (0.272, 0.441) | < .001  | 0.356 (0.272, 0.441) | < .001 
#> x7 ~~ x7           | 0.799 (0.640, 0.959) | < .001  | 0.799 (0.640, 0.959) | < .001 
#> x8 ~~ x8           | 0.488 (0.342, 0.633) | < .001  | 0.488 (0.342, 0.633) | < .001 
#> x9 ~~ x9           | 0.566 (0.427, 0.705) | < .001  | 0.566 (0.427, 0.705) | < .001 
#> visual ~~ visual   | 0.809 (0.524, 1.094) | < .001  | 0.809 (0.524, 1.094) | < .001 
#> textual ~~ textual | 0.979 (0.760, 1.199) | < .001  | 0.774 (0.582, 0.965) | < .001 
#> speed ~~ speed     | 0.384 (0.215, 0.553) | < .001  | 0.297 (0.160, 0.433) | < .001
```

## Override the layout for a specific output

The `select` value stored in the object is the default layout. For
one-off exports or alternate views, override it in
[`format_results()`](https://brianmsm.github.io/psymetrics/reference/format_results.md)
with `table_args`.

``` r
format_results(
  compared_se,
  output = "text",
  table_args = list(select = "{estimate} ({ci})|{p}")
)
#> # Loading
#> 
#> Link          |         CFA          | p (CFA) |         SEM          | p (SEM)
#> -------------------------------------------------------------------------------
#> visual =~ x1  | 1.000 (1.000, 1.000) |         | 1.000 (1.000, 1.000) |        
#> visual =~ x2  | 0.554 (0.358, 0.749) | < .001  | 0.554 (0.358, 0.749) | < .001 
#> visual =~ x3  | 0.729 (0.516, 0.943) | < .001  | 0.729 (0.516, 0.943) | < .001 
#> textual =~ x4 | 1.000 (1.000, 1.000) |         | 1.000 (1.000, 1.000) |        
#> textual =~ x5 | 1.113 (0.985, 1.241) | < .001  | 1.113 (0.985, 1.241) | < .001 
#> textual =~ x6 | 0.926 (0.817, 1.035) | < .001  | 0.926 (0.817, 1.035) | < .001 
#> speed =~ x7   | 1.000 (1.000, 1.000) |         | 1.000 (1.000, 1.000) |        
#> speed =~ x8   | 1.180 (0.857, 1.503) | < .001  | 1.180 (0.857, 1.503) | < .001 
#> speed =~ x9   | 1.082 (0.785, 1.378) | < .001  | 1.082 (0.785, 1.378) | < .001 
#> 
#> # Regression
#> 
#> Link             | CFA | p (CFA) |          SEM          | p (SEM)
#> ------------------------------------------------------------------
#> textual ~ visual |     |         | 0.504 (0.322, 0.687)  | < .001 
#> speed ~ visual   |     |         | 0.297 (0.144, 0.450)  | < .001 
#> speed ~ textual  |     |         | 0.053 (-0.051, 0.158) |  0.318 
#> 
#> # Correlation
#> 
#> Link              |         CFA          | p (CFA) | SEM | p (SEM)
#> ------------------------------------------------------------------
#> visual ~~ textual | 0.408 (0.264, 0.552) | < .001  |     |        
#> visual ~~ speed   | 0.262 (0.152, 0.373) | < .001  |     |        
#> textual ~~ speed  | 0.173 (0.077, 0.270) | < .001  |     |        
#> 
#> # Variance
#> 
#> Link               |         CFA          | p (CFA) |         SEM          | p (SEM)
#> ------------------------------------------------------------------------------------
#> x1 ~~ x1           | 0.549 (0.326, 0.772) | < .001  | 0.549 (0.326, 0.772) | < .001 
#> x2 ~~ x2           | 1.134 (0.934, 1.333) | < .001  | 1.134 (0.934, 1.333) | < .001 
#> x3 ~~ x3           | 0.844 (0.667, 1.022) | < .001  | 0.844 (0.667, 1.022) | < .001 
#> x4 ~~ x4           | 0.371 (0.278, 0.465) | < .001  | 0.371 (0.278, 0.465) | < .001 
#> x5 ~~ x5           | 0.446 (0.332, 0.561) | < .001  | 0.446 (0.332, 0.561) | < .001 
#> x6 ~~ x6           | 0.356 (0.272, 0.441) | < .001  | 0.356 (0.272, 0.441) | < .001 
#> x7 ~~ x7           | 0.799 (0.640, 0.959) | < .001  | 0.799 (0.640, 0.959) | < .001 
#> x8 ~~ x8           | 0.488 (0.342, 0.633) | < .001  | 0.488 (0.342, 0.633) | < .001 
#> x9 ~~ x9           | 0.566 (0.427, 0.705) | < .001  | 0.566 (0.427, 0.705) | < .001 
#> visual ~~ visual   | 0.809 (0.524, 1.094) | < .001  | 0.809 (0.524, 1.094) | < .001 
#> textual ~~ textual | 0.979 (0.760, 1.199) | < .001  | 0.774 (0.582, 0.965) | < .001 
#> speed ~~ speed     | 0.384 (0.215, 0.553) | < .001  | 0.297 (0.160, 0.433) | < .001
```

You can also use custom templates that keep everything in one cell.

``` r
format_results(
  compared_se,
  output = "text",
  table_args = list(select = "{estimate}{stars} ({se})")
)
#> # Loading
#> 
#> Link          |       CFA        |       SEM       
#> ---------------------------------------------------
#> visual =~ x1  |  1.000 (0.000)   |  1.000 (0.000)  
#> visual =~ x2  | 0.554*** (0.100) | 0.554*** (0.100)
#> visual =~ x3  | 0.729*** (0.109) | 0.729*** (0.109)
#> textual =~ x4 |  1.000 (0.000)   |  1.000 (0.000)  
#> textual =~ x5 | 1.113*** (0.065) | 1.113*** (0.065)
#> textual =~ x6 | 0.926*** (0.055) | 0.926*** (0.055)
#> speed =~ x7   |  1.000 (0.000)   |  1.000 (0.000)  
#> speed =~ x8   | 1.180*** (0.165) | 1.180*** (0.165)
#> speed =~ x9   | 1.082*** (0.151) | 1.082*** (0.151)
#> 
#> # Regression
#> 
#> Link             | CFA |       SEM       
#> -----------------------------------------
#> textual ~ visual |     | 0.504*** (0.093)
#> speed ~ visual   |     | 0.297*** (0.078)
#> speed ~ textual  |     |  0.053 (0.053)  
#> 
#> # Correlation
#> 
#> Link              |       CFA        | SEM
#> ------------------------------------------
#> visual ~~ textual | 0.408*** (0.074) |    
#> visual ~~ speed   | 0.262*** (0.056) |    
#> textual ~~ speed  | 0.173*** (0.049) |    
#> 
#> # Variance
#> 
#> Link               |       CFA        |       SEM       
#> --------------------------------------------------------
#> x1 ~~ x1           | 0.549*** (0.114) | 0.549*** (0.114)
#> x2 ~~ x2           | 1.134*** (0.102) | 1.134*** (0.102)
#> x3 ~~ x3           | 0.844*** (0.091) | 0.844*** (0.091)
#> x4 ~~ x4           | 0.371*** (0.048) | 0.371*** (0.048)
#> x5 ~~ x5           | 0.446*** (0.058) | 0.446*** (0.058)
#> x6 ~~ x6           | 0.356*** (0.043) | 0.356*** (0.043)
#> x7 ~~ x7           | 0.799*** (0.081) | 0.799*** (0.081)
#> x8 ~~ x8           | 0.488*** (0.074) | 0.488*** (0.074)
#> x9 ~~ x9           | 0.566*** (0.071) | 0.566*** (0.071)
#> visual ~~ visual   | 0.809*** (0.145) | 0.809*** (0.145)
#> textual ~~ textual | 0.979*** (0.112) | 0.774*** (0.097)
#> speed ~~ speed     | 0.384*** (0.086) | 0.297*** (0.070)
```

## When to use each layer

Use `compare_model_estimates(select = ...)` when you want the object
itself to carry a default presentation.

Use `format_results(..., table_args = list(select = ...))` when you want
a temporary view for a particular report, export, or presentation
without changing the object’s default layout.

## Next steps

- Continue with [Reporting and
  visualization](https://brianmsm.github.io/psymetrics/articles/reporting-and-visualization.md).
- See function-level details in the
  [Reference](https://brianmsm.github.io/psymetrics/reference/index.md).
