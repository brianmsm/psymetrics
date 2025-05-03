# psymetrics (development version)

# psymetrics (development version)

## Bug Fixes and Improvements

-   **Dependency Management**:
    -   Moved `ggplot2` and `lavaan` from `Imports` to `Suggests` to minimize mandatory installation dependencies and better reflect the package's goal of supporting multiple backends. Users now only need these packages if working specifically with `lavaan` objects or associated plotting features. Functions relying on them will now prompt for installation if missing (`rlang::check_installed`).
    -   Added a minimum R version dependency (`R >= 4.1.0`) to DESCRIPTION for compatibility with current R features.

-   **S3 Method Registration**:
    -   Ensured correct and robust S3 method registration for `plot.lavaan`, `print.model_fit`, and `print.compare_model_fit` using `@exportS3Method` with explicit namespaces (`graphics::plot`, `base::print`). This guarantees proper method dispatch by R and avoids exporting the methods directly.

-   **Documentation**:
    -   Added a central conceptual help page (`plot-methods`) documenting the generic `plot` function's usage within the package, linking to available methods (`plot.lavaan`) and future plans.
    -   Improved documentation formatting (line wrapping, clarity) for various functions, including `print.compare_model_fit`.
    -   Added `@Seealso` links across relevant help files (e.g., linking `plot-methods` to `plot.lavaan`, `model_fit` to `model_fit.lavaan`, etc.) for easier navigation.
    -   Updated the package `Title` in DESCRIPTION to better reflect its unifying purpose ("Unified Tools for Psychometric Model Analysis").

-   **Examples**:
    -   Updated all examples that require suggested packages (`lavaan`, `ggplot2`) to run conditionally using `if (requireNamespace(...))`. This prevents errors during `R CMD check` when dependencies are not installed and provides informative messages to users.

-   **Robustness**:
    -   Added internal checks using `rlang::check_installed` at the beginning of functions/methods that require packages now listed in `Suggests` (e.g., `lavaan` in `model_fit.lavaan`, `ggplot2` in `plot_factor_loadings`) to provide clearer error messages if dependencies are missing at runtime.

# psymetrics 0.1.4

## New Features

- **save_table()**: A new utility function for saving tables generated from model outputs to different formats, starting with Word (`.docx`). This function automatically checks for the necessary packages (`flextable` and `officer`) and provides formatting for tables in accordance with APA style. Key features:
  - Default template in landscape orientation.
  - Options to customize the file path and template.
  - Digit precision control, with options for different font styles and cell alignment.

- **plot() method for lavaan objects**: Introduced a `plot` method specifically designed for `lavaan` objects, enabling users to easily visualize key model results. Supported plots:
  - **Factor Loadings Plot** (`type = "loadings"`): Displays standardized factor loadings for CFA models with options to display confidence intervals and adjust axis limits.
  - **Residuals Plot** (`type = "residuals"`) (future): Will display residuals for model diagnostics and evaluation.
  - **Path Diagram Plot** (`type = "path"`) (future): Will generate path diagrams for SEM models.
  
  The factor loadings plot includes parameters for sorting and grouping items, along with flexible display options for confidence intervals and standardized estimates.

## Bug Fixes and Improvements

- **Internal Function Enhancements**: Improved `prepare_table` function for consistent table formatting across different outputs. This includes automatic rounding and character conversion for key columns (`NOBS`, `NPAR`, `Chi2_df`) where applicable.
- **Improved Handling of Global Variables**: Adopted `.data` pronoun within `ggplot2` to avoid unnecessary global variable bindings and improve compatibility with `R CMD check`.

# psymetrics 0.1.3

## New Features

- **Enhanced Output Formats in `print.model_fit` and `print.compare_model_fit`:** Added support for exporting tables in additional formats, including `markdown` and `html`, using the `print.model_fit` and `print.compare_model_fit` functions.
- **Customizable Alignment:** Introduced the `align` argument to control table alignment in text and markdown formats. Available options include "left", "right", "center", and "firstleft". For more details, refer to the `insight::export_table` documentation.
- **Improved Error Handling:** Functions now use `cli::cli_abort` for error handling, providing clearer and more detailed messages when unsupported parameters are passed.

## Improvements

- **Code Refactoring:** Made improvements to the structure of the `print_format` function and related code, optimizing format handling and code clarity.
- **Documentation Update:** The documentation has been updated to reflect new parameters and features, including additional usage examples.

# psymetrics 0.1.2

## New Features

### `compare_model_fit()` Function
- **Added:** New function `compare_model_fit()` allows for comparing fit indices across multiple `lavaan` model objects.
- **Usage:** You can pass multiple fitted `lavaan` model objects to `compare_model_fit()` and it will return a data frame with fit indices for each model, enabling easy comparison.
- **Verbose Option:** Added a `verbose` argument to control the display of informational messages during comparison.
- **Print Method:** Implemented a `print.compare_model_fit` method to provide a formatted output for easy reading of model comparisons.

### Enhanced `model_fit()` Function
- **Updated:** Improved the `model_fit()` function to handle multiple types of fit indices, including "standard", "scaled", and "robust".
- **Custom Metrics:** Introduced the `metrics` argument, allowing users to specify exactly which fit indices they want to extract. If `metrics` is set to "essential", a predefined set of common indices is returned.
- **Verbose Option:** Added a `verbose` argument to control the display of informational messages when metrics are adjusted according to the estimator type.
- **Print Method:** Implemented a `print.model_fit` method to provide a clean and formatted output for the fit indices, with customizable precision.


### Documentation
- **Updated:** Improved the documentation for `model_fit()` and added comprehensive examples showcasing how to use `compare_model_fit()`.

## Note
- **Tests Pending:** Unit tests for `model_fit()` and `compare_model_fit()` will be added in the next update to ensure consistent functionality and output formatting.

# psymetrics 0.1.1

## New Features

- **`print.model_fit` method**: Added to format and display the results of the `model_fit` function.
  - The output format can be customized using the `digits`, `p_digits`, and `format` arguments.
  - Automatically formats the results using `insight::format_table()` and outputs them with `insight::export_table()`.

# psymetrics 0.1.0

## New Features

- **`model_fit.lavaan` function**: Implemented to extract fit indices from models created with `lavaan`.
  - Supports three types of fit indices: `standard`, `scaled`, and `robust`.
  - Allows users to specify custom metrics using the `metrics` argument.
  - Provides informative messages regarding the automatic adjustment of metrics (`scaled` or `robust`) based on the estimator used.

- **`model_fit` function**: Added as a generic function for extracting model fit indices. Currently implemented for objects of class `lavaan`.

## Bug Fixes

- Fixed the S3 method consistency issue between `model_fit` and `model_fit.lavaan`.
- Corrected the declaration of dependencies in the `DESCRIPTION` file to include `rlang` under `Suggests`.

## Notes

- The `NAMESPACE` file is now automatically managed using `roxygen2` to export the appropriate functions and maintain dependency order.
- Added a check in the examples to verify if `lavaan` is installed before running them.

