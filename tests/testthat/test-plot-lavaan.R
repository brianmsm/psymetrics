test_that("plot_factor_loadings returns ggplot", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  plot <- psymetrics::plot_factor_loadings(fit)

  expect_s3_class(plot, "ggplot")
})

test_that("plot.lavaan errors on unsupported type", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  expect_error(plot(fit, type = "unsupported"), "Unsupported plot type")
})

test_that("plot_factor_loadings respects ci alias and ci_bounds messaging", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  expect_silent(psymetrics::plot_factor_loadings(fit, ci = FALSE, ci_bounds = "extend", autofit = FALSE))
  expect_message(
    psymetrics::plot_factor_loadings(fit, CI = FALSE, ci_bounds = "arrow", autofit = FALSE),
    "Ignoring `ci_bounds`"
  )
})

test_that("plot_factor_loadings arrow uses 0-1 when all values are non-negative", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  loadings <- lavaan::standardizedSolution(fit)
  if (!all(c("ci.lower", "ci.upper") %in% names(loadings))) {
    skip("Standardized CIs are unavailable in this lavaan configuration.")
  }
  loadings <- loadings[loadings$op == "=~", ]
  est_col <- if ("est.std" %in% names(loadings)) "est.std" else "est"
  range_values <- c(loadings[[est_col]], loadings$ci.lower, loadings$ci.upper)
  range_values <- range_values[is.finite(range_values)]
  if (!length(range_values) || !all(range_values >= 0 & range_values <= 1)) {
    skip("Loadings/CIs are not all within [0, 1] for this model.")
  }

  plot <- psymetrics::plot_factor_loadings(
    fit,
    ci = TRUE,
    ci_bounds = "arrow",
    autofit = TRUE,
    verbose = FALSE
  )

  expect_equal(plot$coordinates$limits$x, c(0, 1))
})

test_that("plot_factor_loadings warns for non-converged models", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      estimator = "ML",
      control = list(iter.max = 1)
    )
  )

  if (isTRUE(lavaan::lavInspect(fit, "converged"))) {
    skip("Model converged even with iter.max = 1; skipping warning test.")
  }

  expect_warning(
    psymetrics::plot_factor_loadings(fit, verbose = TRUE),
    "did not converge"
  )
})
