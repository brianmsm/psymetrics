# psymetrics: Psychometric Model Fit and Reporting Tools

![psymetrics hex logo](reference/figures/hexlogo.png)

`psymetrics` provides a unified workflow to inspect psychometric model
fit, compare fitted models, extract parameter estimates, generate plots,
and produce publication-ready outputs.

## Current scope

The package currently focuses on `lavaan` CFA and SEM workflows.

- Stable now:
  - Fit extraction with
    [`model_fit()`](https://brianmsm.github.io/psymetrics/reference/model_fit.md)
  - Fit comparison with
    [`compare_model_fit()`](https://brianmsm.github.io/psymetrics/reference/compare_model_fit.md)
  - Fit visualization with
    [`plot_model_fit()`](https://brianmsm.github.io/psymetrics/reference/plot_model_fit.md)
  - Parameter extraction with
    [`model_estimates()`](https://brianmsm.github.io/psymetrics/reference/model_estimates.md)
  - Parameter comparison with
    [`compare_model_estimates()`](https://brianmsm.github.io/psymetrics/reference/compare_model_estimates.md)
  - Reporting with
    [`format_results()`](https://brianmsm.github.io/psymetrics/reference/format_results.md)
    and
    [`save_table()`](https://brianmsm.github.io/psymetrics/reference/save_table.md)
  - Loading visualization with
    [`plot_factor_loadings()`](https://brianmsm.github.io/psymetrics/reference/plot_factor_loadings.md)
- In development:
  - Measurement invariance helpers
  - Additional model-class support (`psych`, `mirt`)

## Install

``` r
# install.packages("pak")
pak::pak("brianmsm/psymetrics@v0.5.0")
```

## Recommended path

1.  Start with the fit workflow article.
2.  Continue with SEM and parameter estimates.
3.  Finish with reporting and visualization patterns.

## Documentation map

- Articles:
  - [Get started with fit
    indices](https://brianmsm.github.io/psymetrics/articles/get-started-fit-indices.md)
  - [SEM and parameter estimates with
    lavaan](https://brianmsm.github.io/psymetrics/articles/sem-and-estimates-lavaan.md)
  - [Reporting and
    visualization](https://brianmsm.github.io/psymetrics/articles/reporting-and-visualization.md)
- API reference:
  [Reference](https://brianmsm.github.io/psymetrics/reference/index.md)
- Changes by release:
  [News](https://brianmsm.github.io/psymetrics/news/index.md)
- Project roadmap:
  [ROADMAP](https://brianmsm.github.io/psymetrics/ROADMAP.md)

## Contributing

Issues and feature requests are welcome:

- Bug reports: include a minimal reproducible example.
- Feature requests: include the target workflow and expected output.

GitHub issues: <https://github.com/brianmsm/psymetrics/issues>
