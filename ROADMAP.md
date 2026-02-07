# Development Roadmap & Versioning Plan

This document outlines the development plan and versioning strategy for the `psymetrics` package. It serves as a portable, high-level guide for development priorities.

While this file contains the overall plan, day-to-day discussion and detailed technical specifications for each task live in the linked GitHub Issues.

## **Current Status & Progress**

* 🚧 In Progress: `v0.2.0` - Extend Existing Functions for SEM

  Current milestone work is focused on official SEM support for existing `lavaan` workflows.

  - **Goal:**
      - Validate and document SEM support in `model_fit()`, `compare_model_fit()`, and `plot_factor_loadings()`.
      - Expand SEM-focused test coverage.
      - Complete acceptance criteria tracked in **[Issue #25](https://github.com/brianmsm/psymetrics/issues/25)**.

* ✅ Completed: `v0.1.9.x` - Stability & Documentation Polish

  - Expanded `testthat` coverage for core workflows and edge cases.
  - Improved formatting/messaging behavior and aligned documentation with current implementation.

* ✅ Completed: `v0.1.5` - Stability & Testing Release

  - Added an initial `testthat` suite covering `model_fit`, `compare_model_fit`, plotting, and `save_table`.

---

## **Future Roadmap**

### **Phase 1: Complete and Consolidate `lavaan` Workflow (CFA & SEM)**

The focus of this phase is to build a robust and feature-complete workflow for models fitted with `lavaan`, covering both Confirmatory Factor Analysis (CFA) and general Structural Equation Models (SEM).

* [ ] **`v0.2.0`**: **Extend Existing Functions for SEM.** *(In Progress)*

  - *Goal*: Officially support SEM in `model_fit`, `compare_model_fit`, and `plot_factor_loadings`.
  - *Details*: **[Issue #25](https://github.com/brianmsm/psymetrics/issues/25)**
  
* [ ] **`v0.3.0`**: Implement `model_estimates()` for parameter extraction.

  - *Details*: **[Issue #17](https://github.com/brianmsm/psymetrics/issues/17)**
  
* [ ] **`v0.4.0`**: Implement `compare_model_estimates()` for parameter comparison.

  - *Details*: **[Issue #18](https://github.com/brianmsm/psymetrics/issues/18)**
  
* [ ] **`v0.5.0`**: Implement `plot_model_fit()` for visualizing fit indices.

  - *Details*: **[Issue #19](https://github.com/brianmsm/psymetrics/issues/19)**
  
* [ ] **`v0.6.0`**: Enhance `compare_model_fit()` for Measurement Invariance (MG-CFA).

  - *Details*: **[Issue #20](https://github.com/brianmsm/psymetrics/issues/20)**
  
* [ ] **`v0.7.0`**: Create a helper function for specifying invariance models.

  - *Details*: **[Issue #21](https://github.com/brianmsm/psymetrics/issues/21)**
  
* [ ] **`v0.8.0`**: Add Excel (`.xlsx`) export functionality to `save_table()`.

  - *Details*: **[Issue #22](https://github.com/brianmsm/psymetrics/issues/22)**
  

### **Phase 2: Expansion to Other Models & Packages**

This phase will commence after the core `lavaan` functionality is stable and complete.

* [ ] **`v0.9.0`**: **Add full support for `lavaan` EFA models.**

  - *Goal*: Extend all relevant functions to be compatible with Exploratory Factor Analysis models fitted with `lavaan`.
  
* [ ] **`v0.10.0`**: **Add full support for `mirt` objects.**

  - *Goal*: Extend all functions to be compatible with models from the `mirt` package.
  
* [ ] **`v0.11.0`**: **Add full support for `psych` EFA objects.**

  - *Goal*: Extend all functions to be compatible with EFA models from the `psych` package.


## **Final Milestone: Stable Release**

* [ ] **`v1.0.0`**: **Stable & Complete Release.**

  - *Goal*: All features are implemented, documented, and thoroughly tested. The package API is considered reliable and stable. Comprehensive documentation, including tutorials (vignettes), is complete.
