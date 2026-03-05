# Reporting and visualization

## Goal

This guide focuses on output workflows:

1.  Prepare comparison tables for markdown reports.
2.  Export fit tables to Word.
3.  Visualize standardized factor loadings.

## Setup

``` r
library(psymetrics)
library(lavaan)

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
fit_ulsm <- cfa(
  model,
  data = HolzingerSwineford1939,
  estimator = "ULSM"
)
fit_table <- compare_model_fit(MLR = fit_mlr, ULSM = fit_ulsm)
```

## Format results for markdown, HTML, and auto output

To request markdown output explicitly:

``` r
format_results(fit_table, output = "markdown")
```

``` markdown
|MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |   CFI |   TLI | RMSEA |      RMSEA  CI |  SRMR |
|:-----|:----:|:---------:|:----:|:--------:|:--------:|:-----:|:-----:|:-----:|:--------------:|:-----:|
|MLR   |  301 |       MLR |   21 |    87.13 |   < .001 | 0.925 | 0.888 | 0.093 | [0.073, 0.115] | 0.065 |
|ULSM  |  301 |      ULSM |   21 |    90.60 |   < .001 | 0.931 | 0.897 | 0.096 | [0.073, 0.120] | 0.059 |
```

In an HTML report/article, running that call renders a formatted table:

| MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA |     RMSEA CI     | SRMR  |
|:------|:----:|:---------:|:----:|:--------:|:--------:|:-----:|:-----:|:-----:|:----------------:|:-----:|
| MLR   | 301  |    MLR    |  21  |  87.13   | \< .001  | 0.925 | 0.888 | 0.093 | \[0.073, 0.115\] | 0.065 |
| ULSM  | 301  |   ULSM    |  21  |  90.60   | \< .001  | 0.931 | 0.897 | 0.096 | \[0.073, 0.120\] | 0.059 |

By default, `format_results(fit_table)` uses `output = "auto"` and
chooses markdown or HTML according to the rendering context:

In HTML-capable contexts, you can explicitly request HTML:

``` r
format_results(fit_table, output = "html")
```

| MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) | CFI   | TLI   | RMSEA | RMSEA CI         | SRMR  |
|-------|------|-----------|------|----------|----------|-------|-------|-------|------------------|-------|
| MLR   | 301  | MLR       | 21   | 87.13    | \< .001  | 0.925 | 0.888 | 0.093 | \[0.073, 0.115\] | 0.065 |
| ULSM  | 301  | ULSM      | 21   | 90.60    | \< .001  | 0.931 | 0.897 | 0.096 | \[0.073, 0.120\] | 0.059 |

## Export to Word

``` r
save_table(
  fit_table,
  path = "model_fit.docx",
  orientation = "landscape"
)
```

## Visualize factor loadings

``` r
plot_factor_loadings(fit_mlr)
```

![Dot plot of standardized factor loadings by item with confidence
intervals.](reporting-and-visualization_files/figure-html/unnamed-chunk-7-1.png)

## Practical notes

- [`format_results()`](https://brianmsm.github.io/psymetrics/reference/format_results.md)
  is useful for reproducible reports.
- [`save_table()`](https://brianmsm.github.io/psymetrics/reference/save_table.md)
  is ideal for manuscript-ready `.docx` tables.
- Use plotting as a communication layer after checking fit and
  parameters.

## Next steps

- Return to [Get started with fit
  indices](https://brianmsm.github.io/psymetrics/articles/get-started-fit-indices.md).
- Explore the
  [Reference](https://brianmsm.github.io/psymetrics/reference/index.md)
  for argument-level control.
