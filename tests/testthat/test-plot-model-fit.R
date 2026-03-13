local_plot_model_fit_objects <- function() {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  hs_data <- lavaan::HolzingerSwineford1939
  model_1 <- "visual =~ x1 + x2 + x3 + x4"
  model_2 <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6"

  fit_1 <- suppressWarnings(
    lavaan::cfa(model_1, data = hs_data, estimator = "MLR")
  )
  fit_2 <- suppressWarnings(
    lavaan::cfa(model_2, data = hs_data, estimator = "MLR")
  )

  list(
    single = psymetrics::model_fit(fit_1),
    compare = psymetrics::compare_model_fit(MLR = fit_1, SEM = fit_2)
  )
}

test_that("plot_model_fit defaults work for model_fit and build successfully", {
  objects <- local_plot_model_fit_objects()

  plot_single <- psymetrics::plot_model_fit(objects$single)

  expect_s3_class(plot_single, "ggplot")
  expect_no_error(ggplot2::ggplot_build(plot_single))
  expect_equal(plot_single$labels$subtitle, "Single-fit bullet chart")
})

test_that("plot_model_fit defaults work for compare_model_fit and preserve model order", {
  objects <- local_plot_model_fit_objects()

  plot_compare <- psymetrics::plot_model_fit(objects$compare)

  expect_s3_class(plot_compare, "ggplot")
  expect_no_error(ggplot2::ggplot_build(plot_compare))
  expect_equal(plot_compare$labels$subtitle, "Threshold-aware dot plot")
  expect_equal(unique(plot_compare$data$MODEL), objects$compare$MODEL)
})

test_that("plot_model_fit supports all valid types for their classes", {
  objects <- local_plot_model_fit_objects()

  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(objects$single, type = "bullet")))
  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(objects$compare, type = "dots")))
  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(objects$compare, type = "bars")))
  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(objects$compare, type = "heatmap")))
})



test_that("plot_model_fit rejects invalid type and class combinations clearly", {
  objects <- local_plot_model_fit_objects()

  expect_error(
    psymetrics::plot_model_fit(objects$single, type = "dots"),
    "Unsupported `type = 'dots'`"
  )
  expect_error(
    psymetrics::plot_model_fit(objects$compare, type = "bullet"),
    "Unsupported `type = 'bullet'`"
  )
})

test_that("plot_model_fit uses canonical metric order when metrics is NULL", {
  fake_single <- data.frame(
    CFI = 0.91,
    RMSEA = 0.07,
    RMSEA_CI_low = 0.05,
    RMSEA_CI_high = 0.09,
    converged = TRUE,
    stringsAsFactors = FALSE
  )
  class(fake_single) <- c("model_fit", "data.frame")

  plot_single <- psymetrics::plot_model_fit(fake_single)
  built_single <- ggplot2::ggplot_build(plot_single)

  expect_equal(
    as.character(built_single[["layout"]][["layout"]][["Metric"]]),
    c("CFI", "RMSEA")
  )
})

test_that("plot_model_fit preserves user metric order for subsets", {
  objects <- local_plot_model_fit_objects()

  plot_single <- psymetrics::plot_model_fit(objects$single, metrics = c("SRMR", "CFI"))
  built_single <- ggplot2::ggplot_build(plot_single)
  expect_equal(
    as.character(built_single[["layout"]][["layout"]][["Metric"]]),
    c("SRMR", "CFI")
  )

  plot_heatmap <- psymetrics::plot_model_fit(objects$compare, type = "heatmap", metrics = c("SRMR", "CFI"))
  expect_equal(levels(plot_heatmap$data$Metric), c("SRMR", "CFI"))
})

test_that("plot_model_fit errors on unsupported metric names", {
  objects <- local_plot_model_fit_objects()

  expect_error(
    psymetrics::plot_model_fit(objects$single, metrics = c("CFI", "AIC")),
    "Unsupported metric name"
  )
})

test_that("plot_model_fit uses RMSEA confidence intervals when available", {
  objects <- local_plot_model_fit_objects()

  single_with_ci <- psymetrics::plot_model_fit(objects$single, metrics = "RMSEA")
  single_without_object <- objects$single
  single_without_object$RMSEA_CI_low <- NULL
  single_without_object$RMSEA_CI_high <- NULL
  single_without_ci <- psymetrics::plot_model_fit(single_without_object, metrics = "RMSEA")

  compare_with_ci <- psymetrics::plot_model_fit(objects$compare, metrics = "RMSEA")
  compare_without_object <- objects$compare
  compare_without_object$RMSEA_CI_low <- NULL
  compare_without_object$RMSEA_CI_high <- NULL
  compare_without_ci <- psymetrics::plot_model_fit(compare_without_object, metrics = "RMSEA")

  grouped_with_ci <- psymetrics::plot_model_fit(objects$compare, type = "bars", metrics = c("RMSEA", "SRMR"))
  grouped_without_ci <- psymetrics::plot_model_fit(compare_without_object, type = "bars", metrics = c("RMSEA", "SRMR"))

  expect_gt(length(single_with_ci$layers), length(single_without_ci$layers))
  expect_gt(length(compare_with_ci$layers), length(compare_without_ci$layers))
  expect_gt(length(grouped_with_ci$layers), length(grouped_without_ci$layers))
  expect_no_error(ggplot2::ggplot_build(single_with_ci))
  expect_no_error(ggplot2::ggplot_build(compare_with_ci))
  expect_no_error(ggplot2::ggplot_build(grouped_with_ci))
})

test_that("plot_model_fit does not crash on non-converged fit objects when metrics exist", {
  fake_single <- data.frame(
    CFI = 0.91,
    TLI = 0.90,
    RMSEA = 0.07,
    RMSEA_CI_low = 0.05,
    RMSEA_CI_high = 0.09,
    SRMR = 0.06,
    converged = FALSE,
    stringsAsFactors = FALSE
  )
  class(fake_single) <- c("model_fit", "data.frame")

  fake_compare <- data.frame(
    MODEL = c("M1", "M2"),
    CFI = c(0.91, 0.94),
    TLI = c(0.90, 0.93),
    RMSEA = c(0.07, 0.06),
    RMSEA_CI_low = c(0.05, 0.04),
    RMSEA_CI_high = c(0.09, 0.08),
    SRMR = c(0.06, 0.05),
    converged = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  class(fake_compare) <- c("compare_model_fit", "model_fit", "data.frame")

  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(fake_single)))
  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(fake_compare)))
})

test_that("plot_model_fit errors for unsupported multi-row summaries and duplicated model rows", {
  objects <- local_plot_model_fit_objects()

  multirow_single <- rbind(objects$single, objects$single)
  class(multirow_single) <- class(objects$single)

  duplicate_compare <- objects$compare[c(1, 1), ]
  class(duplicate_compare) <- class(objects$compare)

  expect_error(
    psymetrics::plot_model_fit(multirow_single),
    "one-row `model_fit` objects only"
  )
  expect_error(
    psymetrics::plot_model_fit(duplicate_compare),
    "one row per model"
  )
})

test_that("plot_model_fit surfaces a clear message when ggplot2 is unavailable", {
  objects <- local_plot_model_fit_objects()

  expect_error(
    testthat::with_mocked_bindings(
      check_installed = function(...) rlang::abort("ggplot2 is required to create model fit plots."),
      psymetrics::plot_model_fit(objects$single),
      .package = "rlang"
    ),
    "ggplot2 is required"
  )
})



