# Generic Function to Extract Model Fit Indices

`model_fit()` is a generic function designed to extract fit indices from
a variety of statistical model objects. It serves as a unified interface
for obtaining key fit metrics, which can vary depending on the type of
model and the specific method implemented for that model class.

- **Confirmatory Factor Analysis (CFA) and Structural Equation Models
  (SEM)**:
  [`model_fit.lavaan()`](https://brianmsm.github.io/psymetrics/reference/model_fit.lavaan.md)

- **Exploratory Factor Analysis** (Future): Future methods may include
  support for models from the `psych` package.

- **IRT models**: Future methods may include support for models from the
  `mirt` package.

## Usage

``` r
model_fit(fit, ...)
```

## Arguments

- fit:

  A model object from which to extract fit indices. The class of this
  object determines the specific method that will be used. For example,
  objects of class `lavaan` will use the method
  [`model_fit.lavaan()`](https://brianmsm.github.io/psymetrics/reference/model_fit.lavaan.md).

- ...:

  Additional arguments passed to the specific method for the model
  class.

## Value

A `data.frame` containing the fit indices of the model. The specific
indices returned depend on the model type and the method used.

## Details

The `model_fit()` function is intended to provide a consistent interface
for extracting fit indices from various types of models. Methods for
specific classes of models should implement the necessary logic to
retrieve and return relevant fit indices in a tidy format.

## See also

[`model_fit.lavaan()`](https://brianmsm.github.io/psymetrics/reference/model_fit.lavaan.md)
for lavaan objects
