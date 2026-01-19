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

test_that("compare_model_fit can include test details columns", {
  skip_if_not_installed("lavaan")

  model1 <- "visual =~ x1 + x2 + x3 + x4"
  model2 <- "visual =~ x1 + x2 + x3 + x4 + x5"

  fit1 <- suppressWarnings(
    lavaan::cfa(model1, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )
  fit2 <- suppressWarnings(
    lavaan::cfa(model2, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )

  combined <- psymetrics::compare_model_fit(fit1, fit2, test_details = TRUE)

  expect_true(all(c("TEST", "SE") %in% names(combined)))
})

test_that("compare_model_fit supports per-model test lists", {
  skip_if_not_installed("lavaan")
  skip_if(packageVersion("lavaan") < "0.6.21", "lavaan < 0.6.21 does not support multi-test options")

  model <- "visual =~ x1 + x2 + x3"

  fit1 <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = c("satorra.bentler", "mean.var.adjusted")
    )
  )
  fit2 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )

  combined <- psymetrics::compare_model_fit(
    fit1,
    fit2,
    test = list(fit1 = c("satorra.bentler", "mean.var.adjusted")),
    standard_test = list(fit1 = TRUE)
  )

  fit1_rows <- nrow(psymetrics::model_fit(
    fit1,
    test = c("satorra.bentler", "mean.var.adjusted"),
    standard_test = TRUE
  ))
  fit2_rows <- nrow(psymetrics::model_fit(fit2))

  expect_equal(nrow(combined), fit1_rows + fit2_rows)
  expect_equal(combined$MODEL, c(rep("fit1", fit1_rows), rep("fit2", fit2_rows)))
})

test_that("compare_model_fit validates named test lists", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3"
  fit1 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )
  fit2 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  expect_error(
    psymetrics::compare_model_fit(fit1, fit2, test = list(c("satorra.bentler"))),
    "named list"
  )

  expect_error(
    psymetrics::compare_model_fit(fit1, fit2, test = list(fit3 = "satorra.bentler")),
    "unknown model names"
  )
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
