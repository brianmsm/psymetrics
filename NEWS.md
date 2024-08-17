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

