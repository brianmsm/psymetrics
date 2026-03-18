local_plot_model_fit_objects <- function() {
  skip_if_not_installed("lavaan")

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

local_plot_model_fit_metadata_objects <- function() {
  skip_if_not_installed("lavaan")

  hs_data <- lavaan::HolzingerSwineford1939
  standard_model <- "visual =~ x1 + x2 + x3 + x4"
  robust_model_1 <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6"
  robust_model_2 <- "visual =~ x1 + x2 + x3 + x4
textual =~ x5 + x6 + x7"

  fit_standard <- suppressWarnings(
    lavaan::cfa(standard_model, data = hs_data, estimator = "ML")
  )
  fit_nonstandard <- suppressWarnings(
    lavaan::cfa(robust_model_1, data = hs_data, estimator = "MLR")
  )
  fit_multi_1 <- suppressWarnings(
    lavaan::cfa(
      robust_model_1,
      data = hs_data,
      estimator = "MLR",
      test = c("satorra.bentler", "mean.var.adjusted")
    )
  )
  fit_multi_2 <- suppressWarnings(
    lavaan::cfa(
      robust_model_2,
      data = hs_data,
      estimator = "MLR",
      test = c("satorra.bentler", "mean.var.adjusted")
    )
  )

  list(
    single_standard = psymetrics::model_fit(fit_standard),
    single_nonstandard = psymetrics::model_fit(fit_nonstandard),
    single_multi = psymetrics::model_fit(fit_multi_1, standard_test = TRUE),
    compare_multi = psymetrics::compare_model_fit(
      First = fit_multi_1,
      Second = fit_multi_2,
      standard_test = TRUE
    )
  )
}

local_plot_model_fit_attach_metadata <- function(x, test_role, is_primary) {
  psymetrics:::model_fit_attach_test_metadata(
    x,
    test_role = test_role,
    is_primary = is_primary
  )
}

local_plot_model_fit_multirow_objects <- function() {
  single <- data.frame(
    ESTIMATOR = c("ML", "MLR", "MLR"),
    TEST = c("standard", "satorra.bentler", "mean.var.adjusted"),
    CFI = c(0.901, 0.925, 0.919),
    TLI = c(0.892, 0.914, 0.909),
    RMSEA = c(0.081, 0.067, 0.072),
    RMSEA_CI_low = c(0.071, 0.055, 0.060),
    RMSEA_CI_high = c(0.091, 0.079, 0.084),
    SRMR = c(0.061, 0.054, 0.056),
    converged = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  class(single) <- c("model_fit", "data.frame")
  single <- local_plot_model_fit_attach_metadata(
    single,
    test_role = c("standard", "non_standard", "non_standard"),
    is_primary = c(FALSE, TRUE, FALSE)
  )

  compare <- data.frame(
    MODEL = c("MLR", "MLR", "ULSM", "ULSM"),
    ESTIMATOR = c("ML", "MLR", "ULS", "ULSM"),
    TEST = c("standard", "satorra.bentler", "standard", "mean.var.adjusted"),
    CFI = c(0.901, 0.925, 0.907, 0.931),
    TLI = c(0.892, 0.914, 0.899, 0.921),
    RMSEA = c(0.081, 0.067, 0.077, 0.059),
    RMSEA_CI_low = c(0.071, 0.055, 0.065, 0.049),
    RMSEA_CI_high = c(0.091, 0.079, 0.089, 0.071),
    SRMR = c(0.061, 0.054, 0.063, 0.050),
    converged = c(TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  class(compare) <- c("compare_model_fit", "model_fit", "data.frame")
  compare <- local_plot_model_fit_attach_metadata(
    compare,
    test_role = c("standard", "non_standard", "standard", "non_standard"),
    is_primary = c(FALSE, TRUE, FALSE, TRUE)
  )

  estimator_only <- data.frame(
    MODEL = c("M1", "M1"),
    ESTIMATOR = c("ML", "MLR"),
    CFI = c(0.90, 0.93),
    TLI = c(0.89, 0.92),
    RMSEA = c(0.08, 0.06),
    RMSEA_CI_low = c(0.07, 0.05),
    RMSEA_CI_high = c(0.09, 0.07),
    SRMR = c(0.06, 0.05),
    converged = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  class(estimator_only) <- c("compare_model_fit", "model_fit", "data.frame")

  duplicate_estimator <- data.frame(
    MODEL = c("M1", "M1"),
    ESTIMATOR = c("MLR", "MLR"),
    CFI = c(0.90, 0.91),
    TLI = c(0.89, 0.90),
    RMSEA = c(0.08, 0.07),
    RMSEA_CI_low = c(0.07, 0.06),
    RMSEA_CI_high = c(0.09, 0.08),
    SRMR = c(0.06, 0.05),
    converged = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  class(duplicate_estimator) <- c("compare_model_fit", "model_fit", "data.frame")

  list(single = single, compare = compare, estimator_only = estimator_only, duplicate_estimator = duplicate_estimator)
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

  compare_df <- psymetrics:::plot_model_fit_prepare_data(
    objects$compare,
    c("CFI", "TLI", "RMSEA", "SRMR"),
    "compare_model_fit",
    "all"
  )
  expect_equal(unique(compare_df$MODEL_BASE), unique(objects$compare$MODEL))
})

test_that("plot_model_fit supports all valid types and bullet only for single-row summaries", {
  objects <- local_plot_model_fit_objects()
  multi <- local_plot_model_fit_multirow_objects()

  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(objects$single, type = "bullet")))
  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(objects$single, type = "dots")))
  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(objects$compare, type = "dots")))
  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(objects$compare, type = "bars")))
  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(objects$compare, type = "heatmap")))
  expect_error(
    psymetrics::plot_model_fit(multi$compare, type = "bullet"),
    "requires a single-row fit summary"
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
  multi <- local_plot_model_fit_multirow_objects()

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

  grouped_with_ci <- psymetrics::plot_model_fit(multi$compare, type = "bars", metrics = c("RMSEA", "SRMR"))
  grouped_without_object <- multi$compare
  grouped_without_object$RMSEA_CI_low <- NULL
  grouped_without_object$RMSEA_CI_high <- NULL
  grouped_without_ci <- psymetrics::plot_model_fit(grouped_without_object, type = "bars", metrics = c("RMSEA", "SRMR"))

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

test_that("plot_model_fit handles multi-row objects through test_mode and default type", {
  multi <- local_plot_model_fit_multirow_objects()

  plot_single_all <- psymetrics::plot_model_fit(multi$single)
  plot_single_primary <- psymetrics::plot_model_fit(multi$single, test_mode = "primary")
  plot_compare_all <- psymetrics::plot_model_fit(multi$compare)

  expect_equal(plot_single_all$labels$subtitle, "Threshold-aware dot plot")
  expect_equal(plot_single_primary$labels$subtitle, "Single-fit bullet chart")
  expect_equal(plot_compare_all$labels$subtitle, "Threshold-aware dot plot")

  single_all_df <- psymetrics:::plot_model_fit_prepare_data(multi$single, c("CFI", "TLI", "RMSEA", "SRMR"), "model_fit", "all")
  compare_all_df <- psymetrics:::plot_model_fit_prepare_data(multi$compare, c("CFI", "TLI", "RMSEA", "SRMR"), "compare_model_fit", "all")
  expect_true(all(single_all_df$PLOT_ID != single_all_df$MODEL_BASE))
  expect_true(all(compare_all_df$PLOT_ID != compare_all_df$MODEL_BASE))
})

test_that("plot_model_fit test_mode filters rows as intended", {
  multi <- local_plot_model_fit_multirow_objects()
  metrics <- c("CFI", "TLI", "RMSEA", "SRMR")

  all_df <- psymetrics:::plot_model_fit_prepare_data(multi$single, metrics, "model_fit", "all")
  non_standard_df <- psymetrics:::plot_model_fit_prepare_data(multi$single, metrics, "model_fit", "non_standard")
  standard_df <- psymetrics:::plot_model_fit_prepare_data(multi$single, metrics, "model_fit", "standard_only")
  primary_df <- psymetrics:::plot_model_fit_prepare_data(multi$single, metrics, "model_fit", "primary")

  expect_equal(nrow(all_df), 3)
  expect_equal(non_standard_df$TEST, c("satorra.bentler", "mean.var.adjusted"))
  expect_equal(standard_df$TEST, "standard")
  expect_equal(primary_df$TEST, "satorra.bentler")
})

test_that("plot_model_fit uses metadata for one-row summaries without TEST", {
  objects <- local_plot_model_fit_metadata_objects()
  metrics <- c("CFI", "TLI", "RMSEA", "SRMR")

  single_standard_df <- psymetrics:::plot_model_fit_prepare_data(objects$single_standard, metrics, "model_fit", "standard_only")
  expect_equal(nrow(single_standard_df), 1)
  expect_equal(single_standard_df$MODEL_BASE, "Model")
  expect_equal(single_standard_df$ESTIMATOR, "ML")

  expect_error(
    psymetrics:::plot_model_fit_prepare_data(objects$single_standard, metrics, "model_fit", "non_standard"),
    "left no rows available to plot"
  )

  single_nonstandard_df <- psymetrics:::plot_model_fit_prepare_data(objects$single_nonstandard, metrics, "model_fit", "non_standard")
  expect_equal(nrow(single_nonstandard_df), 1)
  expect_equal(single_nonstandard_df$ESTIMATOR, "MLR")

  expect_error(
    psymetrics:::plot_model_fit_prepare_data(objects$single_nonstandard, metrics, "model_fit", "standard_only"),
    "left no rows available to plot"
  )
})

test_that("plot_model_fit uses metadata for multi-row summaries without TEST", {
  objects <- local_plot_model_fit_metadata_objects()
  metrics <- c("CFI", "TLI", "RMSEA", "SRMR")

  standard_df <- psymetrics:::plot_model_fit_prepare_data(objects$single_multi, metrics, "model_fit", "standard_only")
  non_standard_df <- psymetrics:::plot_model_fit_prepare_data(objects$single_multi, metrics, "model_fit", "non_standard")
  primary_df <- psymetrics:::plot_model_fit_prepare_data(objects$single_multi, metrics, "model_fit", "primary")

  expect_equal(standard_df$ESTIMATOR, "ML")
  expect_equal(non_standard_df$ESTIMATOR, c("MLR", "satorra.bentler", "mean.var.adjusted"))
  expect_equal(primary_df$ESTIMATOR, "MLR")
})

test_that("plot_model_fit re-aligns metadata after reordering compare_model_fit rows", {
  objects <- local_plot_model_fit_metadata_objects()
  metrics <- c("CFI", "TLI", "RMSEA", "SRMR")

  second_idx <- which(objects$compare_multi$MODEL == "Second")
  first_idx <- which(objects$compare_multi$MODEL == "First")
  reordered_compare <- objects$compare_multi[c(second_idx, first_idx), , drop = FALSE]
  reordered_compare$MODEL <- c(
    rep("Zeta", length(second_idx)),
    rep("Alpha", length(first_idx))
  )

  compare_standard_df <- psymetrics:::plot_model_fit_prepare_data(reordered_compare, metrics, "compare_model_fit", "standard_only")
  compare_primary_df <- psymetrics:::plot_model_fit_prepare_data(reordered_compare, metrics, "compare_model_fit", "primary")

  expect_equal(unique(compare_standard_df$MODEL_BASE), c("Zeta", "Alpha"))
  expect_equal(compare_standard_df$ESTIMATOR, c("ML", "ML"))
  expect_equal(compare_primary_df$ESTIMATOR, c("MLR", "MLR"))
})

test_that("plot_model_fit derives variant labels from TEST, ESTIMATOR, or row fallback", {
  multi <- local_plot_model_fit_multirow_objects()
  metrics <- c("CFI", "TLI", "RMSEA", "SRMR")

  compare_df <- psymetrics:::plot_model_fit_prepare_data(multi$compare, metrics, "compare_model_fit", "all")
  estimator_df <- psymetrics:::plot_model_fit_prepare_data(multi$estimator_only, metrics, "compare_model_fit", "all")
  duplicate_df <- psymetrics:::plot_model_fit_prepare_data(multi$duplicate_estimator, metrics, "compare_model_fit", "all")

  expect_true(all(compare_df$VARIANT %in% c("standard", "satorra.bentler", "mean.var.adjusted")))
  expect_equal(estimator_df$VARIANT, c("ML", "MLR"))
  expect_true(all(grepl("Row", duplicate_df$VARIANT)))
})

test_that("plot_model_fit assigns exact cutoff values to the better CFI/TLI band", {
  band_df <- psymetrics:::plot_model_fit_single_band_spec(c("CFI", "TLI", "RMSEA", "SRMR"))

  expect_equal(psymetrics:::plot_model_fit_assign_band("CFI", 0.90, band_df), "Acceptable")
  expect_equal(psymetrics:::plot_model_fit_assign_band("CFI", 0.95, band_df), "Good")
  expect_equal(psymetrics:::plot_model_fit_assign_band("TLI", 0.90, band_df), "Acceptable")
  expect_equal(psymetrics:::plot_model_fit_assign_band("TLI", 0.95, band_df), "Good")
  expect_equal(psymetrics:::plot_model_fit_assign_band("RMSEA", 0.05, band_df), "Good")
  expect_equal(psymetrics:::plot_model_fit_assign_band("RMSEA", 0.08, band_df), "Acceptable")
})

test_that("plot_model_fit internal auto breaks stay close to the approved plotting behavior", {
  expect_equal(
    psymetrics:::plot_model_fit_auto_breaks("CFI", 0.80, 1.00),
    c(0.80, 0.85, 0.90, 0.95, 1.00)
  )
  expect_equal(
    psymetrics:::plot_model_fit_auto_breaks("TLI", 0.70, 1.00),
    c(0.70, 0.80, 0.90, 1.00)
  )
  expect_equal(
    psymetrics:::plot_model_fit_auto_breaks("RMSEA", 0.00, 0.223),
    c(0.00, 0.05, 0.10, 0.15, 0.20)
  )
})

test_that("plot_model_fit dots uses color by model and keeps variant shapes without a legend", {
  multi <- local_plot_model_fit_multirow_objects()

  plot_compare <- psymetrics::plot_model_fit(multi$compare, type = "dots")
  expect_equal(plot_compare$labels$colour, "Model")
  expect_null(plot_compare$labels$shape)
  expect_true(any(vapply(plot_compare$scales$scales, function(x) "shape" %in% x$aesthetics, logical(1))))
  expect_no_error(ggplot2::ggplot_build(plot_compare))
})

test_that("plot_model_fit bars and heatmap build cleanly for multi-row compare objects", {
  multi <- local_plot_model_fit_multirow_objects()

  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(multi$compare, type = "bars")))
  expect_no_error(ggplot2::ggplot_build(psymetrics::plot_model_fit(multi$compare, type = "heatmap")))
})

test_that("plot_model_fit surfaces guided errors for raw fits and invalid test_mode results", {
  fit <- lm(mpg ~ wt, data = mtcars)
  multi <- local_plot_model_fit_multirow_objects()

  expect_error(
    psymetrics::plot_model_fit(fit),
    "requires a `model_fit` or `compare_model_fit` object, not a raw fitted model"
  )
  expect_no_error(
    psymetrics::plot_model_fit(multi$single, test_mode = "standard_only", type = "bars", metrics = c("CFI"), verbose = FALSE)
  )

  only_non_standard <- multi$single[multi$single$TEST != "standard", , drop = FALSE]
  class(only_non_standard) <- class(multi$single)
  expect_error(
    psymetrics::plot_model_fit(only_non_standard, test_mode = "standard_only"),
    "left no rows available to plot"
  )
})

