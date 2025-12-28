test_that("compare_model_fit combines models with labels", {
  skip_if_not_installed("lavaan")

  model1 <- "visual =~ x1 + x2 + x3 + x4"
  model2 <- "visual =~ x1 + x2 + x3 + x4 + x5"

  fit1 <- suppressWarnings(
    lavaan::cfa(model1, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )
  fit2 <- suppressWarnings(
    lavaan::cfa(model2, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )

  combined <- psymetrics::compare_model_fit(fit1, fit2)

  expect_s3_class(combined, "compare_model_fit")
  expect_equal(nrow(combined), 2)
  expect_equal(combined$MODEL, c("fit1", "fit2"))
})

test_that("compare_model_fit errors with fewer than two models", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  expect_error(
    psymetrics::compare_model_fit(fit),
    "At least two model fits"
  )
})
