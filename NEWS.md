# psymetrics (development version)

## Bug Fixes and Improvements

-   **SEM Support**: Officially documented `model_fit()` and `compare_model_fit()` support for fitted `lavaan` SEM workflows (including CFA, SEM, growth, multigroup, and multilevel variants).
-   **Plotting**: `plot_factor_loadings()` now supports `facet_by = c("none", "group", "level", "group_level")` for SEM contexts with grouping metadata.
-   **Plotting (Breaking)**: `plot_factor_loadings()` now places `verbose` after `facet_by` in the argument order. Positional (unnamed) calls may need to be updated; named arguments are unaffected.
-   **Plotting**: `plot_factor_loadings()` now errors when the fitted `lavaan` object has no measurement loadings (`=~`) instead of returning a silent empty plot.
-   **Model Fit**: Non-converged lavaan fits now return a single-row table with `converged = FALSE` and NA metrics, keeping `compare_model_fit()` stable.
-   **Model Fit**: Non-convergence alerts now respect `verbose` and include model labels when available.
-   **Plotting**: `plot_factor_loadings()` now skips CI error bars when CI values are unavailable and uses data-driven limits when `autofit = TRUE`.
-   **Plotting**: `plot_factor_loadings()` now uses `ci` (accepting legacy `CI` via `...`), adds `ci_bounds` to control standardized CI handling (extend vs arrow), warns on non-converged models, and refines `autofit` limits to keep standardized bounds visible.
-   **Table Export**: `save_table()` aligns default `digits_by_col` for `model_fit`/`compare_model_fit`, accepts case-insensitive `.docx`, and guards alignment for single-column tables.
-   **Messaging**: Standardized the `digits/ci_digits/p_digits` conflict error in English.
-   **Formatting**: `format_results()` now accepts `output_args$align` when top-level `align` is not supplied, errors when both are passed, and rejects `output_args$format` (use `output`).
-   **Model Fit**: Added `model_fit.default()` to return a clear error for unsupported model classes.
-   **Model Fit / Compare Fit**: `test = NULL` is now treated as `"default"` for lavaan flows, with clearer model-specific fallback messaging.
-   **Plotting**: `plot_factor_loadings()` now maps standardized estimates by column name (`est.std`) and aborts with an explicit error if no estimate column is available.
-   **Formatting**: Per-column numeric formatting now recognizes signed and scientific-notation values (for example `1e-05`, `-2E+03`).
-   **Documentation**: Corrected `save_table()` docs to reflect the default APA template font size (Arial 10).

# psymetrics 0.1.9

## Bug Fixes and Improvements

-   **Messaging**: Robust-fit warnings now aggregate per test/estimator, and missing-test messages include the model label.
-   **Model Fit**: Missing requested tests drop rows when `standard_test = FALSE`, and standard-only models preserve their standard `SE` when no extra standard row is added.
-   **Compare Fit**: Per-model test overrides omit models with only missing tests when `standard_test = FALSE`.
-   **Compare Fit**: Per-model overrides now accept named arguments or unambiguous object names, with clearer errors for unknown or duplicate model labels.
-   **Testing**: Expanded `model_fit`/`compare_model_fit` coverage for per-model test lists, missing tests, standard-only fallback, and robust warning aggregation.

# psymetrics 0.1.8

## Bug Fixes and Improvements

-   **Messaging**: Suppressed the standard-index fallback message when `test = "none"` to avoid redundant output before returning NA metrics.
-   **API**: `model_fit.lavaan()` adds `test`, `standard_test`, and `test_details` to control multi-test output; `compare_model_fit()` accepts the same arguments and supports named lists to override per-model selections.
-   **Model Fit**: `model_fit.lavaan()` now reports one row per non-standard test when multiple tests are present; `standard_test` adds the standard-test row first (always standard indices), so the output can include multiple rows per model.
-   **Model Fit**: `test_details = TRUE` adds `TEST` and `SE` columns and retains `_variant` estimator labels for technical output; when `standard_test = TRUE` adds the standard row, `SE` is set to `NA` because fit indices do not depend on standard errors.
-   **Model Fit**: Missing robust fit measures now return `NA` with a warning instead of erroring when `type = "robust"` is requested.
-   **Formatting**: Chi-square df values in `model_fit` and `compare_model_fit` headers now round to 2 decimals when fractional.

# psymetrics 0.1.7

## Bug Fixes and Improvements

-   **lavaan 0.6.21 Compatibility**: Updated `model_fit.lavaan()` to handle multi-element `options$test`, align estimator detection with current test/se defaults, and correctly surface Browne residual defaults for continuous ULS/DWLS fits (including clearer messaging when scaled/robust indices are unavailable).
-   **Estimator Identification**: Expanded detection logic for lavaan estimators when `options$test` is multi-element, covering robust ML variants and WLS/ULS/DWLS cases with Browne residual defaults.
-   **Testing**: Added `model_fit` coverage for Browne residual tests, Bollen-Stine bootstrap tests, `test = "none"`, and robust MLR/WLSMV estimator reporting.

# psymetrics 0.1.6

## API Changes

-   **Table Formatting Pipeline**: `prepare_table()` is now an internal S3 generic focused on producing a base formatted data frame via `insight::format_table()`.
-   **New `format_results()`**: Added an S3 formatting helper that returns text, markdown, or HTML outputs with `table_args`, `output_args`, `digits_by_col`, and `output = "auto"` support.
-   **Print Methods**: `print.model_fit()` and `print.compare_model_fit()` are now console-only text outputs and always return the original object invisibly.
-   **save_table() Signature**: `save_table()` now uses `prepare_table()` and supports `digits_by_col` as a post-processing step, while `table_args` forwards to `insight::format_table()`.
-   **Removed**: `print_format()`, `emit`, `return_object`, and `knitr.in.progress` behavior from the print pipeline.

## Formatting Updates

-   **Integer Handling**: Removed manual rounding and `columns_to_format` handling, relying on `insight::format_table()` for integer-safe formatting.

## Internal

-   **File Organization**: Refactored file layout to separate backend-specific `model_fit.*` methods from result-class methods, with no user-facing changes.

# psymetrics 0.1.5

## Bug Fixes and Improvements

-   **Dependency Management**: Moved `ggplot2` and `lavaan` from `Imports` to `Suggests` to minimize mandatory dependencies. Functions now provide informative prompts if a required package is missing at runtime.

-   **Robustness**: Functions that rely on suggested packages now use internal checks (`rlang::check_installed`) to provide clearer error messages if dependencies are missing.

-   **Examples**: Updated all examples to run conditionally, which prevents errors during `R CMD check` and provides a smoother user experience.

-   **S3 Method Registration**: Ensured correct and robust S3 method registration for `plot` and `print` methods, guaranteeing proper method dispatch by R.

-   **Testing**: Added an initial `testthat` suite covering `model_fit`, `compare_model_fit`, plotting, and `save_table`, with optional dependency checks.

-   **Plotting**: Replaced deprecated `geom_errorbarh()` usage with `geom_errorbar()` + `orientation = "y"` to align with current `ggplot2` guidance.

-   **Table Export & Formatting**: `save_table()` now accepts `"portrait"` (alias `"vertical"`) for template orientation and forwards formatting arguments. `prepare_table()` respects `ci_digits` inputs.

-   **Documentation**:

    -   Added a central help page (`?plot-methods`) to document the generic `plot` function.
    -   Improved navigation between help files with `@Seealso` cross-references.
    -   Updated the package `Title` in `DESCRIPTION` to better reflect its purpose ("Unified Tools for Psychometric Model Analysis").

# psymetrics 0.1.4

## New Features

-   **save_table()**: A new utility function for saving tables generated from model outputs to different formats, starting with Word (`.docx`). This function automatically checks for the necessary packages (`flextable` and `officer`) and provides formatting for tables in accordance with APA style. Key features:

    -   Default template in landscape orientation.
    -   Options to customize the file path and template.
    -   Digit precision control, with options for different font styles and cell alignment.

-   **plot() method for lavaan objects**: Introduced a `plot` method specifically designed for `lavaan` objects, enabling users to easily visualize key model results. Supported plots:

    -   **Factor Loadings Plot** (`type = "loadings"`): Displays standardized factor loadings for CFA models with options to display confidence intervals and adjust axis limits.
    -   **Residuals Plot** (`type = "residuals"`) (future): Will display residuals for model diagnostics and evaluation.
    -   **Path Diagram Plot** (`type = "path"`) (future): Will generate path diagrams for SEM models.

    The factor loadings plot includes parameters for sorting and grouping items, along with flexible display options for confidence intervals and standardized estimates.

## Bug Fixes and Improvements

-   **Internal Function Enhancements**: Improved `prepare_table` function for consistent table formatting across different outputs. This includes automatic rounding and character conversion for key columns (`NOBS`, `NPAR`, `Chi2_df`) where applicable.
-   **Improved Handling of Global Variables**: Adopted `.data` pronoun within `ggplot2` to avoid unnecessary global variable bindings and improve compatibility with `R CMD check`.

# psymetrics 0.1.3

## New Features

-   **Enhanced Output Formats in `print.model_fit` and `print.compare_model_fit`:** Added support for exporting tables in additional formats, including `markdown` and `html`, using the `print.model_fit` and `print.compare_model_fit` functions.
-   **Customizable Alignment:** Introduced the `align` argument to control table alignment in text and markdown formats. Available options include "left", "right", "center", and "firstleft". For more details, refer to the `insight::export_table` documentation.
-   **Improved Error Handling:** Functions now use `cli::cli_abort` for error handling, providing clearer and more detailed messages when unsupported parameters are passed.

## Improvements

-   **Code Refactoring:** Made improvements to the structure of the `print_format` function and related code, optimizing format handling and code clarity.
-   **Documentation Update:** The documentation has been updated to reflect new parameters and features, including additional usage examples.

# psymetrics 0.1.2

## New Features

### `compare_model_fit()` Function

-   **Added:** New function `compare_model_fit()` allows for comparing fit indices across multiple `lavaan` model objects.
-   **Usage:** You can pass multiple fitted `lavaan` model objects to `compare_model_fit()` and it will return a data frame with fit indices for each model, enabling easy comparison.
-   **Verbose Option:** Added a `verbose` argument to control the display of informational messages during comparison.
-   **Print Method:** Implemented a `print.compare_model_fit` method to provide a formatted output for easy reading of model comparisons.

### Enhanced `model_fit()` Function

-   **Updated:** Improved the `model_fit()` function to handle multiple types of fit indices, including "standard", "scaled", and "robust".
-   **Custom Metrics:** Introduced the `metrics` argument, allowing users to specify exactly which fit indices they want to extract. If `metrics` is set to "essential", a predefined set of common indices is returned.
-   **Verbose Option:** Added a `verbose` argument to control the display of informational messages when metrics are adjusted according to the estimator type.
-   **Print Method:** Implemented a `print.model_fit` method to provide a clean and formatted output for the fit indices, with customizable precision.

### Documentation

-   **Updated:** Improved the documentation for `model_fit()` and added comprehensive examples showcasing how to use `compare_model_fit()`.

## Note

-   **Tests Pending:** Unit tests for `model_fit()` and `compare_model_fit()` will be added in the next update to ensure consistent functionality and output formatting.

# psymetrics 0.1.1

## New Features

-   **`print.model_fit` method**: Added to format and display the results of the `model_fit` function.
    -   The output format can be customized using the `digits`, `p_digits`, and `format` arguments.
    -   Automatically formats the results using `insight::format_table()` and outputs them with `insight::export_table()`.

# psymetrics 0.1.0

## New Features

-   **`model_fit.lavaan` function**: Implemented to extract fit indices from models created with `lavaan`.
    -   Supports three types of fit indices: `standard`, `scaled`, and `robust`.
    -   Allows users to specify custom metrics using the `metrics` argument.
    -   Provides informative messages regarding the automatic adjustment of metrics (`scaled` or `robust`) based on the estimator used.
-   **`model_fit` function**: Added as a generic function for extracting model fit indices. Currently implemented for objects of class `lavaan`.

## Bug Fixes

-   Fixed the S3 method consistency issue between `model_fit` and `model_fit.lavaan`.
-   Corrected the declaration of dependencies in the `DESCRIPTION` file to include `rlang` under `Suggests`.

## Notes

-   The `NAMESPACE` file is now automatically managed using `roxygen2` to export the appropriate functions and maintain dependency order.
-   Added a check in the examples to verify if `lavaan` is installed before running them.
