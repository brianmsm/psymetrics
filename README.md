
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
pak::pak("brianmsm/psymetrics@v0.1.4")
```

## Getting Fit Indices

Here is an example of how to use the psymetrics package with a model
created using lavaan.

``` r
library(psymetrics)
library(lavaan)
#> This is lavaan 0.6-19
#> lavaan is FREE software! Please report any bugs.

# Define a simple CFA model
model <- 'visual  =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed   =~ x7 + x8 + x9'

# Fit the model using lavaan
fit <- cfa(model, data = HolzingerSwineford1939, estimator = "MLR")

# Extract and print fit indices
model_fit(fit)
#> NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA |   RMSEA  CI    | SRMR 
#> ----------------------------------------------------------------------------------------------
#> 301  |    MLR    |  21  |  87.132  |  < .001  | 0.925 | 0.888 | 0.093 | [0.073, 0.115] | 0.065

# You can also request specific types of indices, such as 'robust'
model_fit(fit, type = "robust")
#> NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA |   RMSEA  CI    | SRMR 
#> ----------------------------------------------------------------------------------------------
#> 301  |    MLR    |  21  |  87.132  |  < .001  | 0.930 | 0.895 | 0.092 | [0.072, 0.114] | 0.065

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
#> MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |  CFI  |  TLI  | RMSEA |   RMSEA  CI    | SRMR 
#> ------------------------------------------------------------------------------------------------------
#> fit_1 | 301  |    MLR    |  21  |  87.132  |  < .001  | 0.925 | 0.888 | 0.093 | [0.073, 0.115] | 0.065
#> fit_2 | 301  |   ULSM    |  21  |  90.600  |  < .001  | 0.931 | 0.897 | 0.096 | [0.073, 0.120] | 0.059
```

In this example, compare_model_fit is used to compare the fit indices of
two different models. This function allows you to easily see the
differences in model fit across different estimation methods or model
specifications.

## Print the fit indices in HTML format

This is useful when you want to embed the output directly in HTML
reports or web pages.

``` r
print(fit_table, format = "html")
```

| MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) | CFI | TLI | RMSEA | RMSEA CI | SRMR |
|----|----|----|----|----|----|----|----|----|----|----|
| fit_1 | 301 | MLR | 21 | 87.132 | \< .001 | 0.925 | 0.888 | 0.093 | \[0.073, 0.115\] | 0.065 |
| fit_2 | 301 | ULSM | 21 | 90.600 | \< .001 | 0.931 | 0.897 | 0.096 | \[0.073, 0.120\] | 0.059 |

## Print the fit indices in Markdown format

This is ideal for including the output in Markdown documents, such as
GitHub READMEs or R Markdown reports.

``` r
print(fit_table, format = "markdown")
```

    #> |MODEL | NOBS | ESTIMATOR | NPAR | Chi2(24) | p (Chi2) |   CFI |   TLI | RMSEA |      RMSEA  CI |  SRMR |
    #> |:-----|:----:|:---------:|:----:|:--------:|:--------:|:-----:|:-----:|:-----:|:--------------:|:-----:|
    #> |fit_1 |  301 |       MLR |   21 |   87.132 |   < .001 | 0.925 | 0.888 | 0.093 | [0.073, 0.115] | 0.065 |
    #> |fit_2 |  301 |      ULSM |   21 |   90.600 |   < .001 | 0.931 | 0.897 | 0.096 | [0.073, 0.120] | 0.059 |

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

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

In this example, plot_factor_loadings() displays the factor loadings for
each item on the respective factors, with confidence intervals. The plot
can be adjusted to automatically scale the x-axis or group items by
factor.
