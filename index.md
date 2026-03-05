---
title: "psymetrics: Psychometric Model Fit and Reporting Tools"
---

# <span class="home-brand">psymetrics</span>: Psychometric Model Fit and Reporting Tools

<img src="reference/figures/hexlogo.png" alt="psymetrics hex logo" align="right" width="180" class="home-logo" />

`psymetrics` provides a unified workflow to inspect psychometric model fit,
compare fitted models, extract parameter estimates, and generate publication-ready
outputs.

## Current scope

The package currently focuses on `lavaan` CFA and SEM workflows.

- Stable now:
  - Fit extraction with `model_fit()`
  - Fit comparison with `compare_model_fit()`
  - Parameter extraction with `model_estimates()`
  - Reporting with `format_results()` and `save_table()`
  - Loading visualization with `plot_factor_loadings()`
- In development:
  - Measurement invariance helpers
  - Additional model-class support (`psych`, `mirt`)

## Install

```r
# install.packages("pak")
pak::pak("brianmsm/psymetrics@v0.3.0")
```

## Recommended path

1. Start with the fit workflow article.
2. Continue with SEM and parameter estimates.
3. Finish with reporting and visualization patterns.

## Documentation map

- Articles:
  - [Get started with fit indices](articles/get-started-fit-indices.html)
  - [SEM and parameter estimates with lavaan](articles/sem-and-estimates-lavaan.html)
  - [Reporting and visualization](articles/reporting-and-visualization.html)
- API reference: [Reference](reference/index.html)
- Changes by release: [News](news/index.html)
- Project roadmap: [ROADMAP](https://github.com/brianmsm/psymetrics/blob/main/ROADMAP.md)

## Contributing

Issues and feature requests are welcome:

- Bug reports: include a minimal reproducible example.
- Feature requests: include the target workflow and expected output.

GitHub issues: <https://github.com/brianmsm/psymetrics/issues>
