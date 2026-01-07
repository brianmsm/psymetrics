
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psymetrics

<!-- badges: start -->

<!-- badges: end -->

<img src="man/figures/hexlogo.png" align="right" width="200" />

The goal of psymetrics is to provide tools for extracting and
visualizing psychometric model fit indices. It is compatible with models
created using packages like lavaan, psych, and mirt.

## Installation

You can install the development version of psymetrics from
[GitHub](https://github.com/brianmsm/psymetrics) with:

``` r
# install.packages("pak")
pak::pak("brianmsm/psymetrics@v0.1.6")
```

## Getting Fit Indices

Here is an example of how to use the psymetrics package with a model
created using lavaan.

``` r
library(psymetrics)
library(lavaan)
#> This is lavaan 0.6-20
#> lavaan is FREE software! Please report any bugs.

# Define a simple CFA model
model <- 'visual  =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed   =~ x7 + x8 + x9'

# Fit the model using lavaan
fit <- cfa(model, data = HolzingerSwineford1939, estimator = "MLR")

# Extract and print fit indices
model_fit(fit)
#> NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> ---------------------------------------------------------------------
#> 301  |    MLR    |  21  |  87.132  |  < .001  | 0.925 | 0.888 | 0.093
#> 
#> NOBS |   RMSEA  CI    | SRMR 
#> -----------------------------
#> 301  | [0.073, 0.115] | 0.065

# You can also request specific types of indices, such as 'robust'
model_fit(fit, type = "robust")
#> NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> ---------------------------------------------------------------------
#> 301  |    MLR    |  21  |  87.132  |  < .001  | 0.930 | 0.895 | 0.092
#> 
#> NOBS |   RMSEA  CI    | SRMR 
#> -----------------------------
#> 301  | [0.072, 0.114] | 0.065

# Or specify which indices to extract
model_fit(fit, metrics = c("cfi", "tli"))
#> cfi and tli were adjusted to their scaled version.
#> If you want to control the specific metric type used, specify it explicitly
#> (e.g., `cfi.robust`) or modify the type argument.
#> NOBS | ESTIMATOR | NPAR |  CFI  |  TLI 
#> ---------------------------------------
#> 301  |    MLR    |  21  | 0.925 | 0.888
```

This example demonstrates how to extract and print various fit indices
from a confirmatory factor analysis (CFA) model using psymetrics. You
can choose between standard, scaled, or robust fit indices, and even
specify custom sets of indices to extract.

## Comparing Fit Indices

``` r
fit_1 <- cfa(model, data = HolzingerSwineford1939, estimator = "MLR")
fit_2 <- cfa(model, data = HolzingerSwineford1939, estimator = "ULSM")

fit_table <- compare_model_fit(fit_1, fit_2)
fit_table
#> MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA
#> -----------------------------------------------------------------------------
#> fit_1 | 301  |    MLR    |  21  |  87.132  |  < .001  | 0.925 | 0.888 | 0.093
#> fit_2 | 301  |   ULSM    |  21  |  90.600  |  < .001  | 0.931 | 0.897 | 0.096
#> 
#> MODEL |   RMSEA  CI    | SRMR 
#> ------------------------------
#> fit_1 | [0.073, 0.115] | 0.065
#> fit_2 | [0.073, 0.120] | 0.059
```

In this example, compare_model_fit is used to compare the fit indices of
two different models. This function allows you to easily see the
differences in model fit across different estimation methods or model
specifications.

## Format the fit indices in HTML format

This is useful when you want to embed the output directly in HTML
reports or web pages.

``` r
format_results(fit_table, output = "html")
```

<!-- preamble start -->
&#10;    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>
&#10;    <script>
      // Create table-specific functions using external factory
      const tableFns_4pdtyr9kzqvs80vic0ap = TinyTable.createTableFunctions("tinytable_4pdtyr9kzqvs80vic0ap");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '2', j: 1 }, { i: '2', j: 2 }, { i: '2', j: 3 }, { i: '2', j: 4 }, { i: '2', j: 5 }, { i: '2', j: 6 }, { i: '2', j: 7 }, { i: '2', j: 8 }, { i: '2', j: 9 }, { i: '2', j: 10 }, { i: '2', j: 11 } ], css_id: 'tinytable_css_hlozg2wfe37x6tgwlps9',}, 
          { positions: [ { i: '0', j: 1 }, { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 }, { i: '0', j: 5 }, { i: '0', j: 6 }, { i: '0', j: 7 }, { i: '0', j: 8 }, { i: '0', j: 9 }, { i: '0', j: 10 }, { i: '0', j: 11 } ], css_id: 'tinytable_css_wxzltbv7n9821xij6pg7',}, 
          ];
&#10;          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_4pdtyr9kzqvs80vic0ap.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>
&#10;    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_4pdtyr9kzqvs80vic0ap td.tinytable_css_hlozg2wfe37x6tgwlps9, #tinytable_4pdtyr9kzqvs80vic0ap th.tinytable_css_hlozg2wfe37x6tgwlps9 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%;  }
    #tinytable_4pdtyr9kzqvs80vic0ap td.tinytable_css_wxzltbv7n9821xij6pg7, #tinytable_4pdtyr9kzqvs80vic0ap th.tinytable_css_wxzltbv7n9821xij6pg7 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%;  }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_4pdtyr9kzqvs80vic0ap" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        &#10;        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1">MODEL</th>
                <th scope="col" data-row="0" data-col="2">NOBS</th>
                <th scope="col" data-row="0" data-col="3">ESTIMATOR</th>
                <th scope="col" data-row="0" data-col="4">NPAR</th>
                <th scope="col" data-row="0" data-col="5">Chi2(24)</th>
                <th scope="col" data-row="0" data-col="6">p (Chi2)</th>
                <th scope="col" data-row="0" data-col="7">CFI</th>
                <th scope="col" data-row="0" data-col="8">TLI</th>
                <th scope="col" data-row="0" data-col="9">RMSEA</th>
                <th scope="col" data-row="0" data-col="10">RMSEA  CI</th>
                <th scope="col" data-row="0" data-col="11">SRMR</th>
              </tr>
        </thead>
        &#10;        <tbody>
                <tr>
                  <td data-row="1" data-col="1">fit_1</td>
                  <td data-row="1" data-col="2">301</td>
                  <td data-row="1" data-col="3">MLR</td>
                  <td data-row="1" data-col="4">21</td>
                  <td data-row="1" data-col="5">87.132</td>
                  <td data-row="1" data-col="6">< .001</td>
                  <td data-row="1" data-col="7">0.925</td>
                  <td data-row="1" data-col="8">0.888</td>
                  <td data-row="1" data-col="9">0.093</td>
                  <td data-row="1" data-col="10">[0.073, 0.115]</td>
                  <td data-row="1" data-col="11">0.065</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">fit_2</td>
                  <td data-row="2" data-col="2">301</td>
                  <td data-row="2" data-col="3">ULSM</td>
                  <td data-row="2" data-col="4">21</td>
                  <td data-row="2" data-col="5">90.600</td>
                  <td data-row="2" data-col="6">< .001</td>
                  <td data-row="2" data-col="7">0.931</td>
                  <td data-row="2" data-col="8">0.897</td>
                  <td data-row="2" data-col="9">0.096</td>
                  <td data-row="2" data-col="10">[0.073, 0.120]</td>
                  <td data-row="2" data-col="11">0.059</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->

## Format the fit indices in Markdown format

This is ideal for including the output in Markdown documents, such as
GitHub READMEs or R Markdown reports.

``` r
format_results(fit_table, output = "markdown")
```

| MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) | CFI | TLI | RMSEA | RMSEA CI | SRMR |
|:---|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| fit_1 | 301 | MLR | 21 | 87.132 | \< .001 | 0.925 | 0.888 | 0.093 | \[0.073, 0.115\] | 0.065 |
| fit_2 | 301 | ULSM | 21 | 90.600 | \< .001 | 0.931 | 0.897 | 0.096 | \[0.073, 0.120\] | 0.059 |

## Saving Fit Indices to Word

The `save_table()` function allows you to export the fit indices to a
Word document (.docx) with APA-style formatting and optional templates
for vertical or landscape orientation.

``` r
# Save the fit comparison table to Word
save_table(fit_table, path = "model_fit.docx", orientation = "landscape")
```

The exported document will have a clean and professional format that you
can directly include in reports or presentations.

## Plotting Factor Loadings

You can visualize the factor loadings of your model with the
`plot_factor_loadings()` function. This function creates a dot plot of
standardized factor loadings, with the option to display confidence
intervals for each loading.

``` r
plot_factor_loadings(fit)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

In this example, plot_factor_loadings() displays the factor loadings for
each item on the respective factors, with confidence intervals. The plot
can be adjusted to automatically scale the x-axis or group items by
factor.

------------------------------------------------------------------------

## Project Roadmap

This is a summary of the development plan for `psymetrics`. The
immediate focus is to build a comprehensive and robust workflow for
models fitted with the `lavaan` package, including **Confirmatory Factor
Analysis (CFA) and Structural Equation Models (SEM)**.

Our current work involves a stability and testing release (`v0.1.6`) to
solidify the existing codebase. The complete **Development Roadmap &
Versioning Plan**, which tracks our progress version by version, is
available in the **[ROADMAP.md](ROADMAP.md)** file and **[Issue
\#23](https://github.com/brianmsm/psymetrics/issues/23)**.

For technical details on a specific future feature, please see the
corresponding issue link below.

### **Phase 1: Consolidate and Extend `lavaan` Features (CFA & SEM)**

The following features are planned to enhance the workflow for **CFA and
SEM**.

- **Parameter Analysis:**
  - [ ] **New Function:** Create `model_estimates()` to extract model
    parameters from `lavaan` models. [(Issue
    \#17)](https://github.com/brianmsm/psymetrics/issues/17)
  - [ ] **New Function:** Create `compare_model_estimates()` to compare
    parameters between two or more `lavaan` CFA models. [(Issue
    \#18)](https://github.com/brianmsm/psymetrics/issues/18)
- **Visualization:**
  - [x] Plot factor loadings (`plot_factor_loadings`).
  - [ ] **New Function:** Create a new function `plot_model_fit()` to
    visualize and compare fit indices across different models. [(Issue
    \#19)](https://github.com/brianmsm/psymetrics/issues/19)
- **Fit Analysis & Invariance:**
  - [x] Extract fit indices (`model_fit`) from `lavaan` models.
  - [x] Compare fit indices (`compare_model_fit`) between `lavaan`
    models.
  - [ ] Enhance `compare_model_fit` for measurement invariance (MG-CFA)
    by automatically computing fit difference metrics (e.g., ΔCFI).
    [(Issue \#20)](https://github.com/brianmsm/psymetrics/issues/20)
  - [ ] Add a new helper function to simplify the process of specifying
    nested invariance models (e.g., configural, metric, scalar). [(Issue
    \#21)](https://github.com/brianmsm/psymetrics/issues/21)
- **Exporting:**
  - [x] Export tables to Word (`.docx`), HTML, and Markdown.
  - [ ] Add option to export tables to Excel (`.xlsx`). [(Issue
    \#22)](https://github.com/brianmsm/psymetrics/issues/22)

### **Phase 2: Future Expansion to EFA & IRT**

- [ ] Extend core functions (`model_fit`, `compare_model_fit`,
  `model_estimates`, `compare_model_estimates`) to be compatible with:
  - Exploratory Factor Analysis (EFA) models from `psych` and `lavaan`.
  - Item Response Theory (IRT) models from `mirt`.
- [ ] Extend visualization functions (`plot_factor_loadings`,
  `plot_model_fit`) to be compatible with EFA and IRT models.
