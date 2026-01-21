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

  model <- "visual =~ x1 + x2 + x3 + x4"

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

  capture.output(
    combined <- psymetrics::compare_model_fit(
      fit1,
      fit2,
      test = list(fit1 = c("satorra.bentler", "mean.var.adjusted")),
      standard_test = list(fit1 = TRUE)
    ),
    type = "message"
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

test_that("compare_model_fit accepts object-name aliases for named arguments", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3 + x4"

  fit1 <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = "mean.var.adjusted"
    )
  )
  fit2 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )

  capture.output(
    combined <- psymetrics::compare_model_fit(
      M1 = fit1,
      fit2,
      test = list(fit1 = "mean.var.adjusted"),
      standard_test = list(fit1 = TRUE),
      test_details = TRUE
    ),
    type = "message"
  )

  fit1_rows <- nrow(psymetrics::model_fit(
    fit1,
    test = "mean.var.adjusted",
    standard_test = TRUE,
    test_details = TRUE
  ))
  fit2_rows <- nrow(psymetrics::model_fit(fit2))

  expect_equal(nrow(combined), fit1_rows + fit2_rows)
  expect_equal(combined$MODEL, c(rep("M1", fit1_rows), rep("fit2", fit2_rows)))
})

test_that("compare_model_fit drops models when requested tests are missing and standard_test is FALSE", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6"

  fit1 <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = c("satorra.bentler", "mean.var.adjusted")
    )
  )
  fit2 <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = "mean.var.adjusted"
    )
  )

  messages <- capture.output(
    combined <- psymetrics::compare_model_fit(
      fit1,
      fit2,
      test = list(
        fit1 = c("satorra.bentler", "mean.var.adjusted"),
        fit2 = "satorra.bentler"
      ),
      standard_test = FALSE,
      test_details = TRUE
    ),
    type = "message"
  )

  expect_true(any(grepl(
    "Requested tests not found in the fit2 model and were dropped: satorra.bentler\\.",
    messages
  )))
  expect_false(any(grepl("No requested non-standard tests are available", messages)))
  expect_false("fit2" %in% combined$MODEL)
  expect_true("fit1" %in% combined$MODEL)
})

test_that("compare_model_fit standard rows report TEST and SE when requested", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit1 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )
  fit2 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )

  capture.output(
    combined <- psymetrics::compare_model_fit(
      fit1,
      fit2,
      type = "robust",
      standard_test = TRUE,
      test_details = TRUE
    ),
    type = "message"
  )

  expect_true(all(c("TEST", "SE") %in% names(combined)))

  fit1_rows <- combined[combined$MODEL == "fit1", ]
  fit2_rows <- combined[combined$MODEL == "fit2", ]

  expect_equal(fit1_rows$TEST[1], "standard")
  expect_true(is.na(fit1_rows$SE[1]))
  expect_true(any(!is.na(fit1_rows$SE[-1])))

  expect_equal(fit2_rows$TEST[1], "standard")
  expect_true(is.na(fit2_rows$SE[1]))
  expect_true(any(!is.na(fit2_rows$SE[-1])))
})

test_that("compare_model_fit keeps standard-only models when no overrides are requested", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6"

  fit1 <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = c("satorra.bentler", "mean.var.adjusted")
    )
  )
  fit2 <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = "mean.var.adjusted"
    )
  )
  fit3 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )
  fit4 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  messages <- capture.output(
    combined <- psymetrics::compare_model_fit(
      fit1,
      fit2,
      fit3,
      fit4,
      test = list(
        fit1 = c("satorra.bentler", "mean.var.adjusted")
      ),
      standard_test = FALSE,
      type = "robust",
      test_details = TRUE
    ),
    type = "message"
  )

  expect_true(any(grepl(
    "The fit4 model reports only standard tests; using standard indices instead\\.",
    messages
  )))
  expect_true("fit4" %in% combined$MODEL)
  fit4_rows <- combined[combined$MODEL == "fit4", ]
  expect_equal(nrow(fit4_rows), 1)
  expect_equal(fit4_rows$TEST, "standard")
  expect_equal(fit4_rows$SE, lavaan::lavInspect(fit4, "options")$se)
})

test_that("compare_model_fit emits standard-test message once", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit1 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )
  fit2 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )

  messages <- capture.output(
    invisible(psymetrics::compare_model_fit(
      fit1,
      fit2,
      type = "robust",
      standard_test = TRUE
    )),
    type = "message"
  )

  message_count <- sum(grepl(
    "Standard-test row uses standard indices for estimator",
    messages
  ))
  expect_equal(message_count, 1)
})

test_that("compare_model_fit aggregates robust warning messages", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6"

  fit1 <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = "mean.var.adjusted"
    )
  )
  fit2 <- suppressWarnings(
    lavaan::cfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      test = "mean.var.adjusted"
    )
  )

  robust_targets <- c("cfi.robust", "tli.robust", "rmsea.robust")
  fit_measures <- lavaan::fitmeasures(
    fit1,
    fm.args = list(scaled.test = "mean.var.adjusted")
  )
  robust_values <- fit_measures[robust_targets]
  if (length(robust_values) > 0L && all(!is.na(robust_values))) {
    skip("Robust fit measures are available for mean.var.adjusted in this lavaan version.")
  }

  messages <- capture.output(
    invisible(psymetrics::compare_model_fit(
      fit1,
      fit2,
      test = list(fit1 = "mean.var.adjusted", fit2 = "mean.var.adjusted"),
      standard_test = FALSE,
      type = "robust"
    )),
    type = "message"
  )

  message_count <- sum(grepl("Robust fit measures are not available", messages))
  expect_equal(message_count, 1)
  expect_true(any(grepl("mean\\.var\\.adjusted \\(.+\\)", messages)))
})

test_that("compare_model_fit validates named test lists", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3 + x4"
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
    "Unknown model name"
  )
})

test_that("compare_model_fit errors when multiple list names target one model", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit2 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )
  fit3 <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  expect_error(
    psymetrics::compare_model_fit(
      fit1 = fit2,
      fit3,
      test = list(fit1 = "satorra.bentler", fit2 = "mean.var.adjusted")
    ),
    "multiple entries for model"
  )
})

test_that("compare_model_fit errors with fewer than two models", {
  skip_if_not_installed("lavaan")

  model <- "visual =~ x1 + x2 + x3 + x4"
  fit <- suppressWarnings(
    lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  expect_error(
    psymetrics::compare_model_fit(fit),
    "At least two model fits"
  )
})
