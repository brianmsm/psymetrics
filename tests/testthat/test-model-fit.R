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

test_that("model_fit falls back to standard indices without robust estimator", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  result <- suppressMessages(
    suppressWarnings(psymetrics::model_fit(fit, type = "scaled"))
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$ESTIMATOR, "ML")
  expect_false(is.na(result$Chi2))
})

test_that("model_fit preserves MLF estimator labeling for ML first-order information", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3"
  fit <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      estimator = "ML",
      information = "first.order"
    )
  )

  result <- suppressMessages(
    psymetrics::model_fit(fit)
  )

  expect_equal(result$ESTIMATOR, "MLF")
})

test_that("model_fit returns one row per non-standard test", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6"
  fit <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = c("satorra.bentler", "mean.var.adjusted")
    )
  )

  result <- suppressMessages(
    psymetrics::model_fit(fit)
  )

  expect_equal(nrow(result), 2)
})

test_that("model_fit uses test names for unknown estimator variants", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6"
  fit <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = c("satorra.bentler", "mean.var.adjusted")
    )
  )

  result <- suppressMessages(
    psymetrics::model_fit(fit)
  )

  expect_equal(result$ESTIMATOR, c("satorra.bentler", "mean.var.adjusted"))
})

test_that("model_fit can include test and se details for lavaan", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6"
  fit <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = c("satorra.bentler", "mean.var.adjusted")
    )
  )

  result <- suppressMessages(
    psymetrics::model_fit(fit, test_details = TRUE)
  )

  expect_true(all(c("TEST", "SE") %in% names(result)))
  expect_equal(result$TEST, c("satorra.bentler", "mean.var.adjusted"))
  expect_true(all(result$ESTIMATOR == "ML_variant"))
  expect_true(all(result$SE == lavaan::lavInspect(fit, "options")$se))
})

test_that("model_fit handles missing robust measures for multi-test fits", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6"
  fit <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = c("satorra.bentler", "mean.var.adjusted")
    )
  )

  result <- suppressWarnings(
    suppressMessages(psymetrics::model_fit(fit, type = "robust"))
  )

  expect_s3_class(result, "model_fit")
  expect_equal(nrow(result), 2)
})

test_that("model_fit can include the standard test row first", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6"
  fit <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = c("satorra.bentler", "mean.var.adjusted")
    )
  )

  result <- suppressMessages(
    psymetrics::model_fit(fit, standard_test = TRUE)
  )

  expect_equal(nrow(result), 3)
  expect_equal(result$ESTIMATOR[1], lavaan::lavInspect(fit, "options")$estimator)
})

test_that("model_fit handles Browne residual tests for ULS", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ULS")
  )

  result <- suppressMessages(
    psymetrics::model_fit(fit, type = "scaled")
  )

  expect_equal(result$ESTIMATOR, "ULS")
  expect_false(is.na(result$Chi2))
})

test_that("model_fit handles Browne residual tests for DWLS", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "DWLS")
  )

  result <- suppressMessages(
    psymetrics::model_fit(fit, type = "scaled")
  )

  expect_equal(result$ESTIMATOR, "DWLS")
  expect_false(is.na(result$Chi2))
})

test_that("model_fit treats Bollen-Stine tests as standard-only", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9"
  fit <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      estimator = "ML",
      test = "bollen.stine",
      bootstrap = 20
    )
  )

  result <- suppressMessages(
    psymetrics::model_fit(fit, type = "scaled")
  )

  expect_equal(result$ESTIMATOR, "ML")
  expect_false(is.na(result$Chi2))
})

test_that("model_fit returns NA metrics when test is none", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, test = "none")
  )

  result <- suppressMessages(
    psymetrics::model_fit(fit, type = "scaled")
  )

  expect_equal(result$ESTIMATOR, "ML")
  expect_true(all(is.na(result$Chi2)))
  expect_true(all(is.na(result$CFI)))
})

test_that("model_fit reports robust MLR estimators", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )

  result <- suppressMessages(
    psymetrics::model_fit(fit, type = "robust")
  )

  expect_equal(result$ESTIMATOR, "MLR")
  expect_false(is.na(result$Chi2))
})

test_that("model_fit reports robust WLSMV estimators", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9"
  data <- lavaan::HolzingerSwineford1939
  ordered_vars <- paste0("x", 1:9)
  data[ordered_vars] <- lapply(data[ordered_vars], function(x) {
    cut(x, breaks = 3, include.lowest = TRUE, ordered_result = TRUE)
  })

  fit <- suppressWarnings(
    lavaan::cfa(model, data = data, estimator = "WLSMV", ordered = ordered_vars)
  )

  result <- suppressMessages(
    psymetrics::model_fit(fit, type = "robust")
  )

  expect_equal(result$ESTIMATOR, "WLSMV")
  expect_false(is.na(result$Chi2))
})
