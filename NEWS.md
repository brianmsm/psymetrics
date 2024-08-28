# psymetrics 0.1.3

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

