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
