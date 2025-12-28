test_that("model_fit returns expected columns for lavaan", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  result <- psymetrics::model_fit(fit)

  expect_s3_class(result, "model_fit")
  expect_true(all(c("NOBS", "ESTIMATOR", "NPAR") %in% names(result)))
})

test_that("model_fit handles standard indices with robust estimator", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )

  result <- suppressMessages(
    suppressWarnings(psymetrics::model_fit(fit, type = "standard"))
  )

  expect_s3_class(result, "model_fit")
  expect_gt(ncol(result), 0)
})

test_that("model_fit returns empty data for scaled indices without robust estimator", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  result <- suppressMessages(
    suppressWarnings(psymetrics::model_fit(fit, type = "scaled"))
  )

  expect_equal(ncol(result), 0)
  expect_equal(nrow(result), 0)
})

test_that("print.model_fit rejects unsupported formats", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  result <- psymetrics::model_fit(fit)

  expect_error(print(result, format = "nope"), "Unsupported format")
})
