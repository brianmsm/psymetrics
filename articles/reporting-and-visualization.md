# Reporting and visualization

## Goal

This guide focuses on communication-ready output workflows:

1.  Format fit results for markdown and HTML reports.
2.  Export fit tables to Word.
3.  Visualize single-model and multi-model fit summaries with
    [`plot_model_fit()`](https://brianmsm.github.io/psymetrics/reference/plot_model_fit.md).
4.  Use
    [`plot_factor_loadings()`](https://brianmsm.github.io/psymetrics/reference/plot_factor_loadings.md)
    as a complementary structural view.

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
fit_mlr_multi <- cfa(
  model,
  data = HolzingerSwineford1939,
  estimator = "MLR",
  test = c("satorra.bentler", "mean.var.adjusted")
)

single_fit <- model_fit(fit_mlr)
compared_fits <- compare_model_fit(MLR = fit_mlr, ULSM = fit_ulsm)
single_fit_multi <- model_fit(fit_mlr_multi, standard_test = TRUE)
```

## Format results for markdown, HTML, and auto output

To request markdown output explicitly:

``` r
format_results(compared_fits, output = "markdown")
```

``` markdown
|MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |   CFI |   TLI | RMSEA |      RMSEA  CI |  SRMR |
|:-----|:----:|:---------:|:----:|:--------:|:--------:|:-----:|:-----:|:-----:|:--------------:|:-----:|
|MLR   |  301 |       MLR |   21 |    87.13 |   < .001 | 0.925 | 0.888 | 0.093 | [0.073, 0.115] | 0.065 |
|ULSM  |  301 |      ULSM |   21 |    90.60 |   < .001 | 0.931 | 0.897 | 0.096 | [0.073, 0.120] | 0.059 |
```

In an HTML report or pkgdown article, the same object can render
directly as a formatted table:

| MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA |     RMSEA CI     | SRMR  |
|:------|:----:|:---------:|:----:|:--------:|:--------:|:-----:|:-----:|:-----:|:----------------:|:-----:|
| MLR   | 301  |    MLR    |  21  |  87.13   | \< .001  | 0.925 | 0.888 | 0.093 | \[0.073, 0.115\] | 0.065 |
| ULSM  | 301  |   ULSM    |  21  |  90.60   | \< .001  | 0.931 | 0.897 | 0.096 | \[0.073, 0.120\] | 0.059 |

By default, `format_results(compared_fits)` uses `output = "auto"` and
chooses markdown or HTML according to the rendering context.

In HTML-capable contexts, you can also request HTML explicitly:

``` r
format_results(compared_fits, output = "html")
```

| MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) | CFI   | TLI   | RMSEA | RMSEA CI         | SRMR  |
|-------|------|-----------|------|----------|----------|-------|-------|-------|------------------|-------|
| MLR   | 301  | MLR       | 21   | 87.13    | \< .001  | 0.925 | 0.888 | 0.093 | \[0.073, 0.115\] | 0.065 |
| ULSM  | 301  | ULSM      | 21   | 90.60    | \< .001  | 0.931 | 0.897 | 0.096 | \[0.073, 0.120\] | 0.059 |

## Export to Word

``` r
save_table(
  compared_fits,
  path = "model_fit.docx",
  orientation = "landscape"
)
```

## Visualize a single fitted model

[`plot_model_fit()`](https://brianmsm.github.io/psymetrics/reference/plot_model_fit.md)
is the public plotting entrypoint for
[`model_fit()`](https://brianmsm.github.io/psymetrics/reference/model_fit.md)
and
[`compare_model_fit()`](https://brianmsm.github.io/psymetrics/reference/compare_model_fit.md)
objects. For a one-row `model_fit` summary, the default style resolves
to the single-fit bullet chart.

``` r
plot_model_fit(single_fit)
```

![Bullet chart of CFI, TLI, RMSEA, and SRMR for a single fitted
model.](reporting-and-visualization_files/figure-html/unnamed-chunk-7-1.png)

## Visualize a model comparison

For
[`compare_model_fit()`](https://brianmsm.github.io/psymetrics/reference/compare_model_fit.md)
objects, the default plot is the threshold-aware dot plot, which works
well as a quick side-by-side comparison.

``` r
plot_model_fit(compared_fits)
```

![Threshold-aware dot plot comparing CFI, TLI, RMSEA, and SRMR across
two fitted
models.](reporting-and-visualization_files/figure-html/unnamed-chunk-8-1.png)

## Try an alternative plot style

In `v0.5.0`, the supported plot styles are `default`, `bullet`, `dots`,
`bars`, and `heatmap`. The article keeps to the main workflow, while the
[plot_model_fit
reference](https://brianmsm.github.io/psymetrics/reference/plot_model_fit.md)
documents the full argument surface and supported constraints.

``` r
plot_model_fit(compared_fits, type = "bars")
```

![Grouped threshold bar chart comparing fit indices for two fitted
models.](reporting-and-visualization_files/figure-html/unnamed-chunk-9-1.png)

## Focus on selected metrics

If you only want to communicate part of the fit summary, pass a subset
with `metrics`.

``` r
plot_model_fit(
  compared_fits,
  type = "bars",
  metrics = c("CFI", "TLI", "RMSEA")
)
```

![Grouped threshold bar chart using only CFI, TLI, and
RMSEA.](reporting-and-visualization_files/figure-html/unnamed-chunk-10-1.png)

## Work with multiple test rows

When the fit object keeps both standard and non-standard test summaries,
[`plot_model_fit()`](https://brianmsm.github.io/psymetrics/reference/plot_model_fit.md)
can filter which rows to show with `test_mode`. Here,
`standard_test = TRUE` keeps multiple rows in the
[`model_fit()`](https://brianmsm.github.io/psymetrics/reference/model_fit.md)
result, and `test_mode = "primary"` reduces the view to the primary
non-standard summary.

``` r
plot_model_fit(single_fit_multi, test_mode = "primary")
```

![Bullet chart for the primary non-standard fit summary selected from a
multi-row model_fit
object.](reporting-and-visualization_files/figure-html/unnamed-chunk-11-1.png)

## Visualize factor loadings as a complementary check

[`plot_model_fit()`](https://brianmsm.github.io/psymetrics/reference/plot_model_fit.md)
answers how well the model fits overall.
[`plot_factor_loadings()`](https://brianmsm.github.io/psymetrics/reference/plot_factor_loadings.md)
adds a complementary view of the measurement structure after the fit
summary looks acceptable.

``` r
plot_factor_loadings(fit_mlr)
```

![Dot plot of standardized factor loadings by item with confidence
intervals.](reporting-and-visualization_files/figure-html/unnamed-chunk-12-1.png)

## Practical notes

- Use
  [`format_results()`](https://brianmsm.github.io/psymetrics/reference/format_results.md)
  when you want the same fit object rendered in reports.
- Use
  [`save_table()`](https://brianmsm.github.io/psymetrics/reference/save_table.md)
  when the output needs to go into a `.docx` workflow.
- Use
  [`plot_model_fit()`](https://brianmsm.github.io/psymetrics/reference/plot_model_fit.md)
  for the supported `CFI`/`TLI`/`RMSEA`/`SRMR` workflows in `v0.5.0`,
  and keep the reference page nearby for argument-level detail.
- Use
  [`plot_factor_loadings()`](https://brianmsm.github.io/psymetrics/reference/plot_factor_loadings.md)
  after fit checking when you want to discuss item structure rather than
  overall fit.

## Next steps

- Return to [Get started with fit
  indices](https://brianmsm.github.io/psymetrics/articles/get-started-fit-indices.md).
- Continue with [SEM and parameter
  estimates](https://brianmsm.github.io/psymetrics/articles/sem-and-estimates-lavaan.md).
- Explore the
  [Reference](https://brianmsm.github.io/psymetrics/reference/index.md)
  for argument-level control.
