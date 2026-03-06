# Development Roadmap & Versioning Plan

This document is the high-level roadmap for `psymetrics`. It is meant to
answer two questions quickly:

1.  What is already done?
2.  What is the next milestone?

Detailed technical discussion for each milestone lives in the linked
GitHub Issues.

## Status at a glance

- ✅ Latest completed milestone: `v0.4.0` - Implement
  [`compare_model_estimates()`](https://brianmsm.github.io/psymetrics/reference/compare_model_estimates.md)
  - Added parameter-level comparison for fitted `lavaan` models.
  - Integrated with [`print()`](https://rdrr.io/r/base/print.html),
    [`format_results()`](https://brianmsm.github.io/psymetrics/reference/format_results.md),
    and
    [`save_table()`](https://brianmsm.github.io/psymetrics/reference/save_table.md).
  - Expanded tests and pkgdown-facing documentation.
  - Tracked in **[Issue
    \#18](https://github.com/brianmsm/psymetrics/issues/18)**.
- ⏭ Next planned milestone: `v0.5.0` - Implement `plot_model_fit()`
  - Goal: add a visual workflow for fit indices.
  - Tracked in **[Issue
    \#19](https://github.com/brianmsm/psymetrics/issues/19)**.

## Recently completed milestones

- ✅ **`v0.4.0`** - Implement
  [`compare_model_estimates()`](https://brianmsm.github.io/psymetrics/reference/compare_model_estimates.md)
  *(completed March 6, 2026)*
- ✅ **`v0.3.0`** - Implement
  [`model_estimates()`](https://brianmsm.github.io/psymetrics/reference/model_estimates.md)
  *(completed February 25, 2026)*
- ✅ **`v0.2.0`** - Extend SEM support across existing workflows
  *(completed February 8, 2026)*

Earlier stabilization milestones (`v0.1.9.x`, `v0.1.5`) focused on
testing, messaging, and documentation polish.

------------------------------------------------------------------------

## Roadmap by phase

### Phase 1: Complete and consolidate the `lavaan` workflow

The goal of this phase is a robust, end-to-end CFA/SEM workflow for
models fitted with `lavaan`.

**`v0.2.0`**: Extend existing functions for SEM

- Officially support SEM in
  [`model_fit()`](https://brianmsm.github.io/psymetrics/reference/model_fit.md),
  [`compare_model_fit()`](https://brianmsm.github.io/psymetrics/reference/compare_model_fit.md),
  and
  [`plot_factor_loadings()`](https://brianmsm.github.io/psymetrics/reference/plot_factor_loadings.md).
- Details: **[Issue
  \#25](https://github.com/brianmsm/psymetrics/issues/25)**

**`v0.3.0`**: Implement
[`model_estimates()`](https://brianmsm.github.io/psymetrics/reference/model_estimates.md)

- Add parameter extraction for `lavaan` CFA/SEM workflows.
- Details: **[Issue
  \#17](https://github.com/brianmsm/psymetrics/issues/17)**

**`v0.4.0`**: Implement
[`compare_model_estimates()`](https://brianmsm.github.io/psymetrics/reference/compare_model_estimates.md)

- Add parameter comparison across two or more fitted models.
- Details: **[Issue
  \#18](https://github.com/brianmsm/psymetrics/issues/18)**

**`v0.5.0`**: Implement `plot_model_fit()`

- Add visual summaries for fit indices.
- Details: **[Issue
  \#19](https://github.com/brianmsm/psymetrics/issues/19)**

**`v0.6.0`**: Enhance
[`compare_model_fit()`](https://brianmsm.github.io/psymetrics/reference/compare_model_fit.md)
for measurement invariance (MG-CFA)

- Details: **[Issue
  \#20](https://github.com/brianmsm/psymetrics/issues/20)**

**`v0.7.0`**: Create a helper for specifying invariance models

- Details: **[Issue
  \#21](https://github.com/brianmsm/psymetrics/issues/21)**

**`v0.8.0`**: Add Excel (`.xlsx`) export support to
[`save_table()`](https://brianmsm.github.io/psymetrics/reference/save_table.md)

- Details: **[Issue
  \#22](https://github.com/brianmsm/psymetrics/issues/22)**

### Phase 2: Expand support to other model classes

This phase starts after the core `lavaan` workflow is stable.

**`v0.9.0`**: Add full support for `lavaan` EFA models

**`v0.10.0`**: Add full support for `mirt` objects

**`v0.11.0`**: Add full support for `psych` EFA objects

## Final milestone

**`v1.0.0`**: Stable and complete release

- All planned core features are implemented, documented, and thoroughly
  tested.
- The package API is considered reliable and stable.
- Tutorials and long-form documentation are complete.
