sem_hs_model <- "
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
textual ~ visual
speed ~ visual + textual
"

sem_hs_model_alt <- "
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
textual ~ visual
speed ~ textual
"

sem_political_model <- "
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
dem60 ~ ind60
dem65 ~ ind60 + dem60
"

sem_twolevel_model <- "
level: 1
  fw =~ y1 + y2 + y3
  fw ~ x1 + x2 + x3
level: 2
  fb =~ y1 + y2 + y3
  fb ~ w1 + w2
"

growth_model <- "
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
"

test_that("canonical PoliticalDemocracy SEM works across core functions", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  fit_sem_mlr <- suppressWarnings(
    lavaan::sem(sem_political_model, data = lavaan::PoliticalDemocracy, estimator = "MLR")
  )
  fit_sem_ml <- suppressWarnings(
    lavaan::sem(sem_political_model, data = lavaan::PoliticalDemocracy, estimator = "ML")
  )

  fit_tbl <- suppressMessages(
    psymetrics::model_fit(fit_sem_mlr, test_details = TRUE)
  )
  cmp_tbl <- suppressMessages(
    psymetrics::compare_model_fit(MLR = fit_sem_mlr, ML = fit_sem_ml, test_details = TRUE)
  )
  p <- psymetrics::plot_factor_loadings(fit_sem_mlr, verbose = FALSE)

  expect_s3_class(fit_tbl, "model_fit")
  expect_true(all(c("TEST", "SE") %in% names(fit_tbl)))
  expect_s3_class(cmp_tbl, "compare_model_fit")
  expect_true(all(c("MODEL", "TEST", "SE") %in% names(cmp_tbl)))
  expect_s3_class(p, "ggplot")
  expect_gt(nrow(p$data), 0)
})

test_that("SEM with structural paths works for model_fit and compare_model_fit", {
  skip_if_not_installed("lavaan")

  fit_mlr <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )
  fit_alt <- suppressWarnings(
    lavaan::sem(sem_hs_model_alt, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  out <- suppressMessages(psymetrics::model_fit(fit_mlr, type = "robust"))
  cmp <- suppressMessages(
    psymetrics::compare_model_fit(main = fit_mlr, alt = fit_alt, test_details = TRUE)
  )

  expect_s3_class(out, "model_fit")
  expect_equal(nrow(out), 1)
  expect_equal(out$ESTIMATOR, "MLR")
  expect_s3_class(cmp, "compare_model_fit")
  expect_equal(sort(unique(cmp$MODEL)), c("alt", "main"))
})

test_that("SEM robust ML estimators return valid model_fit outputs", {
  skip_if_not_installed("lavaan")

  estimators <- c("MLR", "MLM", "MLMV", "MLMVS")
  ran_any <- FALSE

  for (estimator in estimators) {
    fit <- tryCatch(
      suppressWarnings(
        lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = estimator)
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      next
    }

    ran_any <- TRUE
    out <- suppressMessages(psymetrics::model_fit(fit, type = "robust"))
    expect_s3_class(out, "model_fit")
    expect_gte(nrow(out), 1)
    expect_true(all(c("NOBS", "ESTIMATOR", "NPAR") %in% names(out)))
  }

  if (!ran_any) {
    skip("No robust ML SEM estimators were available in this lavaan setup.")
  }
})

test_that("categorical SEM with WLSMV works across fit and plotting", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  hs_ord <- lavaan::HolzingerSwineford1939
  ordered_vars <- paste0("x", 1:9)
  hs_ord[ordered_vars] <- lapply(hs_ord[ordered_vars], function(x) {
    cut(x, breaks = 3, include.lowest = TRUE, ordered_result = TRUE)
  })

  fit <- suppressWarnings(
    lavaan::sem(
      sem_hs_model,
      data = hs_ord,
      estimator = "WLSMV",
      ordered = ordered_vars
    )
  )

  out <- suppressMessages(psymetrics::model_fit(fit, type = "robust"))
  p <- psymetrics::plot_factor_loadings(fit, verbose = FALSE)

  expect_s3_class(out, "model_fit")
  expect_equal(out$ESTIMATOR, "WLSMV")
  expect_s3_class(p, "ggplot")
  expect_gt(nrow(p$data), 0)
})

test_that("multigroup SEM supports facet_by = group", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  fit <- suppressWarnings(
    lavaan::sem(
      sem_hs_model,
      data = lavaan::HolzingerSwineford1939,
      group = "school"
    )
  )

  p <- psymetrics::plot_factor_loadings(fit, facet_by = "group", verbose = FALSE)
  expected_group_labels <- lavaan::lavInspect(fit, "group.label")

  expect_s3_class(p, "ggplot")
  expect_true("group" %in% names(p$data))
  expect_gt(length(unique(as.character(p$data$group))), 1)
  expect_setequal(unique(as.character(p$data$group)), expected_group_labels)
  expect_true(inherits(p$facet, "FacetWrap"))
})

test_that("multilevel SEM supports facet_by = level", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  fit <- suppressWarnings(
    lavaan::sem(
      sem_twolevel_model,
      data = lavaan::Demo.twolevel,
      cluster = "cluster"
    )
  )

  p <- psymetrics::plot_factor_loadings(fit, facet_by = "level", verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true("level" %in% names(p$data))
  expect_true(inherits(p$facet, "FacetWrap"))
})

test_that("multilevel SEM works with model_fit and compare_model_fit", {
  skip_if_not_installed("lavaan")

  fit_twolevel <- suppressWarnings(
    lavaan::sem(
      sem_twolevel_model,
      data = lavaan::Demo.twolevel,
      cluster = "cluster"
    )
  )
  fit_ref <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  out <- suppressMessages(psymetrics::model_fit(fit_twolevel, test_details = TRUE, verbose = FALSE))
  cmp <- suppressMessages(
    psymetrics::compare_model_fit(
      ref = fit_ref,
      twolevel = fit_twolevel,
      test_details = TRUE,
      verbose = FALSE
    )
  )

  expect_s3_class(out, "model_fit")
  expect_true(all(c("TEST", "SE") %in% names(out)))
  expect_s3_class(cmp, "compare_model_fit")
  expect_true(all(c("MODEL", "TEST", "SE") %in% names(cmp)))
  expect_true(all(c("ref", "twolevel") %in% cmp$MODEL))
})

test_that("SEM with missing = fiml returns valid fit summaries", {
  skip_if_not_installed("lavaan")

  hs_missing <- lavaan::HolzingerSwineford1939
  set.seed(123)
  hs_missing[sample(seq_len(nrow(hs_missing)), size = 70), "x1"] <- NA
  hs_missing[sample(seq_len(nrow(hs_missing)), size = 60), "x5"] <- NA

  fit_missing <- suppressWarnings(
    lavaan::sem(
      sem_hs_model,
      data = hs_missing,
      missing = "fiml",
      estimator = "MLR"
    )
  )

  out <- suppressMessages(psymetrics::model_fit(fit_missing, test_details = TRUE))

  expect_s3_class(out, "model_fit")
  expect_equal(nrow(out), 1)
  expect_true(all(c("TEST", "SE") %in% names(out)))
  expect_false(is.na(out$NOBS))
})

test_that("SEM with test = none returns NA fit metrics and supports compare_model_fit", {
  skip_if_not_installed("lavaan")

  fit_none <- suppressWarnings(
    lavaan::sem(
      sem_hs_model,
      data = lavaan::HolzingerSwineford1939,
      test = "none"
    )
  )
  fit_ref <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  out <- suppressMessages(psymetrics::model_fit(fit_none, type = "scaled", test_details = TRUE))
  cmp <- suppressMessages(
    psymetrics::compare_model_fit(
      ref = fit_ref,
      none = fit_none,
      type = "scaled",
      test_details = TRUE,
      verbose = FALSE
    )
  )

  expect_s3_class(out, "model_fit")
  expect_true(all(c("TEST", "SE") %in% names(out)))
  expect_equal(out$TEST, "none")
  expect_true(all(is.na(out$Chi2)))
  expect_true(all(is.na(out$CFI)))

  expect_s3_class(cmp, "compare_model_fit")
  expect_true(all(c("ref", "none") %in% cmp$MODEL))
  none_rows <- cmp[cmp$MODEL == "none", ]
  expect_true(all(is.na(none_rows$Chi2)))
})

test_that("SEM with Bollen-Stine test falls back to standard metrics", {
  skip_if_not_installed("lavaan")

  fit_boot <- suppressWarnings(
    lavaan::sem(
      sem_hs_model,
      data = lavaan::HolzingerSwineford1939,
      estimator = "ML",
      test = "bollen.stine",
      bootstrap = 20
    )
  )
  fit_ref <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )

  out <- suppressMessages(psymetrics::model_fit(fit_boot, type = "scaled", test_details = TRUE))
  cmp <- suppressMessages(
    psymetrics::compare_model_fit(
      ref = fit_ref,
      boot = fit_boot,
      type = "scaled",
      test_details = TRUE,
      verbose = FALSE
    )
  )

  expect_s3_class(out, "model_fit")
  expect_equal(out$ESTIMATOR, "ML")
  expect_true(out$TEST %in% c("bollen.stine", "standard"))
  expect_false(is.na(out$Chi2))

  expect_s3_class(cmp, "compare_model_fit")
  expect_true(all(c("ref", "boot") %in% cmp$MODEL))
  boot_rows <- cmp[cmp$MODEL == "boot", ]
  expect_true(all(boot_rows$TEST %in% c("bollen.stine", "standard")))
  expect_true(all(boot_rows$ESTIMATOR == "ML"))
  expect_true(all(!is.na(boot_rows$Chi2)))
})

test_that("non-converged SEM keeps stable outputs for model_fit and compare_model_fit", {
  skip_if_not_installed("lavaan")

  fit_bad <- suppressWarnings(
    lavaan::sem(
      sem_hs_model,
      data = lavaan::HolzingerSwineford1939[1:20, ],
      estimator = "MLR",
      control = list(iter.max = 1)
    )
  )
  if (isTRUE(lavaan::lavInspect(fit_bad, "converged"))) {
    skip("Model converged unexpectedly with iter.max = 1.")
  }

  fit_ok <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )

  out <- suppressMessages(psymetrics::model_fit(fit_bad, test_details = TRUE, verbose = FALSE))
  cmp <- suppressMessages(
    psymetrics::compare_model_fit(ok = fit_ok, bad = fit_bad, test_details = TRUE, verbose = FALSE)
  )

  expect_false(out$converged)
  for (col in intersect(c("Chi2", "Chi2_df", "p_Chi2"), names(out))) {
    expect_true(is.na(out[[col]]))
  }
  bad_rows <- cmp[cmp$MODEL == "bad", ]
  expect_false(any(bad_rows$converged))
})

test_that("growth models are supported by model_fit and plot_factor_loadings", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  fit <- suppressWarnings(
    lavaan::growth(growth_model, data = lavaan::Demo.growth)
  )

  out <- suppressMessages(psymetrics::model_fit(fit))
  p <- psymetrics::plot_factor_loadings(fit, verbose = FALSE)

  expect_s3_class(out, "model_fit")
  expect_equal(nrow(out), 1)
  expect_s3_class(p, "ggplot")
  expect_gt(nrow(p$data), 0)
})

test_that("growth models work with compare_model_fit", {
  skip_if_not_installed("lavaan")

  fit_growth <- suppressWarnings(
    lavaan::growth(growth_model, data = lavaan::Demo.growth)
  )
  fit_ref <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  cmp <- suppressMessages(
    psymetrics::compare_model_fit(
      ref = fit_ref,
      growth = fit_growth,
      test_details = TRUE,
      verbose = FALSE
    )
  )

  expect_s3_class(cmp, "compare_model_fit")
  expect_true(all(c("MODEL", "TEST", "SE") %in% names(cmp)))
  expect_true(all(c("ref", "growth") %in% cmp$MODEL))
})

test_that("path-only SEM errors in plot_factor_loadings and plot.lavaan", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  path_data <- transform(mtcars, x1 = wt, x2 = hp, y = mpg)
  fit_path <- suppressWarnings(
    lavaan::sem("y ~ x1 + x2\nx1 ~~ x2", data = path_data)
  )
  fit_ref <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  out <- suppressMessages(psymetrics::model_fit(fit_path, test_details = TRUE, verbose = FALSE))
  cmp <- suppressMessages(
    psymetrics::compare_model_fit(
      ref = fit_ref,
      path = fit_path,
      test_details = TRUE,
      verbose = FALSE
    )
  )

  expect_s3_class(out, "model_fit")
  expect_equal(nrow(out), 1)
  expect_true(all(c("TEST", "SE") %in% names(out)))
  expect_s3_class(cmp, "compare_model_fit")
  expect_true(all(c("ref", "path") %in% cmp$MODEL))

  expect_error(
    psymetrics::plot_factor_loadings(fit_path),
    "does not contain measurement loadings"
  )
  expect_error(
    plot(fit_path),
    "does not contain measurement loadings"
  )
})

test_that("facet_by falls back with informative message when metadata is unavailable", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  fit <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  expect_message(
    p <- psymetrics::plot_factor_loadings(
      fit,
      facet_by = "group_level",
      verbose = TRUE
    ),
    "Ignoring `facet_by = \"group_level\"`"
  )
  expect_s3_class(p, "ggplot")
})

test_that("facet_by fallback stays silent when verbose = FALSE", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  fit <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )

  expect_silent(
    p <- psymetrics::plot_factor_loadings(
      fit,
      facet_by = "group_level",
      verbose = FALSE
    )
  )
  expect_s3_class(p, "ggplot")
})

test_that("compare_model_fit supports mixed SEM configurations with test_details", {
  skip_if_not_installed("lavaan")

  fit_ml <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = "ML")
  )
  fit_mlr <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )
  fit_mg <- suppressWarnings(
    lavaan::sem(sem_hs_model, data = lavaan::HolzingerSwineford1939, group = "school")
  )

  cmp <- suppressMessages(
    psymetrics::compare_model_fit(
      ml = fit_ml,
      mlr = fit_mlr,
      mg = fit_mg,
      type = "robust",
      test_details = TRUE,
      verbose = FALSE
    )
  )

  expect_s3_class(cmp, "compare_model_fit")
  expect_true(all(c("MODEL", "TEST", "SE") %in% names(cmp)))
  expect_true(all(c("ml", "mlr", "mg") %in% cmp$MODEL))
})
