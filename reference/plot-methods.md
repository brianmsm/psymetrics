# Generic Plot Function for Model Visualizations

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) is a generic
function designed for visualizing various aspects of statistical models.
It provides a unified interface for generating plots that vary depending
on the model type and the specific method implemented for that model
class.

## Value

A plot object, usually a ggplot, specific to the model type.

## Details

Current available method:

- **`lavaan` CFA and SEM models**:
  [`plot.lavaan()`](https://brianmsm.github.io/psymetrics/reference/plot.lavaan.md)
  for plotting factor loadings and other metrics for models from the
  `lavaan` package.

Future methods may include support for:

- **Exploratory Factor Analysis**: Models from the `psych` package.

- **Item Response Theory (IRT)**: Models from the `mirt` package.

## See also

[`plot.lavaan()`](https://brianmsm.github.io/psymetrics/reference/plot.lavaan.md)
for lavaan objects
