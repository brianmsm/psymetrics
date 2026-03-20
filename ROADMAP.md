# Development Roadmap & Versioning Plan

This document is the high-level roadmap for `psymetrics`. It is meant to answer two questions quickly:

1. What is already done?
2. What is the next milestone?

Detailed technical discussion for each milestone lives in the linked GitHub Issues.

## Status at a glance

- ✅ Latest completed milestone: `v0.5.0` - Implement `plot_model_fit()`
  - Added visual summaries for `model_fit()` and `compare_model_fit()` results.
  - Implemented the default single-fit and multi-fit plot styles plus two alternative multi-fit layouts.
  - Initial plotting support currently targets the documented `CFI`/`TLI`/`RMSEA`/`SRMR` workflows; future work should expand advanced documented customization, additional fit indices, and broader support for more customized `model_fit()` / `compare_model_fit()` objects.
  - Expanded tests and pkgdown-facing documentation.
  - Tracked in **[Issue #19](https://github.com/brianmsm/psymetrics/issues/19)**.

- ⏭ Next planned milestone: `v0.6.0` - Enhance `compare_model_fit()` for measurement invariance (MG-CFA)
  - Goal: improve invariance-oriented fit comparison workflows.
  - Tracked in **[Issue #20](https://github.com/brianmsm/psymetrics/issues/20)**.

## Recently completed milestones
- ✅ **`v0.5.0`** - Implement `plot_model_fit()` *(completed March 13, 2026)*
- ✅ **`v0.4.0`** - Implement `compare_model_estimates()` *(completed March 6, 2026)*
- ✅ **`v0.3.0`** - Implement `model_estimates()` *(completed February 25, 2026)*
- ✅ **`v0.2.0`** - Extend SEM support across existing workflows *(completed February 8, 2026)*

Earlier stabilization milestones (`v0.1.9.x`, `v0.1.5`) focused on testing, messaging, and documentation polish.

---

## Roadmap by phase

### Phase 1: Complete and consolidate the `lavaan` workflow

The goal of this phase is a robust, end-to-end CFA/SEM workflow for models fitted with `lavaan`.

- [x] **`v0.2.0`**: Extend existing functions for SEM
  - Officially support SEM in `model_fit()`, `compare_model_fit()`, and `plot_factor_loadings()`.
  - Details: **[Issue #25](https://github.com/brianmsm/psymetrics/issues/25)**

- [x] **`v0.3.0`**: Implement `model_estimates()`
  - Add parameter extraction for `lavaan` CFA/SEM workflows.
  - Details: **[Issue #17](https://github.com/brianmsm/psymetrics/issues/17)**

- [x] **`v0.4.0`**: Implement `compare_model_estimates()`
  - Add parameter comparison across two or more fitted models.
  - Details: **[Issue #18](https://github.com/brianmsm/psymetrics/issues/18)**

- [x] **`v0.5.0`**: Implement `plot_model_fit()`
  - Add visual summaries for fit indices.
  - Initial support centers on the documented `CFI`/`TLI`/`RMSEA`/`SRMR` plotting workflows; future enhancements should cover advanced user-facing customization, additional fit indices, and more customized `model_fit()` / `compare_model_fit()` signatures.
  - Details: **[Issue #19](https://github.com/brianmsm/psymetrics/issues/19)**

- [ ] **`v0.6.0`**: Enhance `compare_model_fit()` for measurement invariance (MG-CFA)
  - Details: **[Issue #20](https://github.com/brianmsm/psymetrics/issues/20)**

- [ ] **`v0.7.0`**: Create a helper for specifying invariance models
  - Details: **[Issue #21](https://github.com/brianmsm/psymetrics/issues/21)**

- [ ] **`v0.8.0`**: Add Excel (`.xlsx`) export support to `save_table()`
  - Details: **[Issue #22](https://github.com/brianmsm/psymetrics/issues/22)**

### Phase 2: Expand support to other model classes

This phase starts after the core `lavaan` workflow is stable.

- [ ] **`v0.9.0`**: Add full support for `lavaan` EFA models
- [ ] **`v0.10.0`**: Add full support for `mirt` objects
- [ ] **`v0.11.0`**: Add full support for `psych` EFA objects

## Final milestone

- [ ] **`v1.0.0`**: Stable and complete release
  - All planned core features are implemented, documented, and thoroughly tested.
  - The package API is considered reliable and stable.
  - Tutorials and long-form documentation are complete.
