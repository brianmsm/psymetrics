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

test_that("plot_factor_loadings supports standardized = FALSE", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  expected <- lavaan::parameterEstimates(fit)
  expected <- expected[expected$op == "=~", "est", drop = TRUE]

  plot <- psymetrics::plot_factor_loadings(
    fit,
    standardized = FALSE,
    sort = FALSE,
    group_by = FALSE,
    ci = FALSE
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(sort(plot$data$est), sort(expected), tolerance = 1e-8)
})

test_that("plot_factor_loadings supports group_by = FALSE", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  plot <- psymetrics::plot_factor_loadings(
    fit,
    group_by = FALSE,
    sort = FALSE,
    ci = FALSE
  )

  expect_true(all(as.character(plot$data$Factor) == "All Items"))
})

test_that("plot_factor_loadings supports sort = FALSE", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  expected <- lavaan::standardizedSolution(fit)
  expected <- expected[expected$op == "=~", "rhs", drop = TRUE]

  plot <- psymetrics::plot_factor_loadings(
    fit,
    sort = FALSE,
    group_by = FALSE,
    ci = FALSE
  )

  expect_equal(as.character(plot$data$rhs), expected)
})

test_that("plot_factor_loadings maps est.std to est by name", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  standardized <- lavaan::standardizedSolution(fit)
  if (!"est.std" %in% names(standardized)) {
    skip("The current lavaan output does not provide est.std.")
  }
  expected <- standardized[standardized$op == "=~", "est.std", drop = TRUE]

  plot <- psymetrics::plot_factor_loadings(
    fit,
    sort = FALSE,
    group_by = FALSE,
    ci = FALSE
  )

  expect_true("est" %in% names(plot$data))
  expect_equal(sort(plot$data$est), sort(expected), tolerance = 1e-8)
})

test_that("plot_factor_loadings errors for lavaan models without =~ loadings", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  path_data <- transform(mtcars, x1 = wt, x2 = hp, y = mpg)
  fit <- suppressWarnings(
    lavaan::sem("y ~ x1 + x2\nx1 ~~ x2", data = path_data)
  )

  expect_error(
    psymetrics::plot_factor_loadings(fit),
    "does not contain measurement loadings"
  )
})
