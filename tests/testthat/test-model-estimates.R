hs_cfa_model <- "
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
"

hs_sem_model <- "
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
textual ~ visual
speed ~ visual + textual
"

hs_sem_defined_model <- "
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
textual ~~ speed
textual ~ a*visual
speed ~ b*visual
indirect := a*b
"

test_that("model_estimates returns class and base columns", {
  skip_if_not_installed("lavaan")

  fit <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  out <- suppressMessages(psymetrics::model_estimates(fit, verbose = FALSE))

  expect_s3_class(out, "model_estimates")
  expect_true(all(c(
    "To", "Operator", "From", "Coefficient", "SE", "CI_low", "CI_high",
    "z", "p", "Component", "converged"
  ) %in% names(out)))
  expect_true(any(out$Component == "Loading"))
})

test_that("model_estimates supports SEM extraction and component filtering", {
  skip_if_not_installed("lavaan")

  fit <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )

  out_all <- suppressMessages(psymetrics::model_estimates(fit, verbose = FALSE))
  out_loading <- suppressMessages(
    psymetrics::model_estimates(fit, component = "loading", verbose = FALSE)
  )
  out_lr <- suppressMessages(
    psymetrics::model_estimates(
      fit,
      component = c("loading", "regression"),
      verbose = FALSE
    )
  )

  expect_true(all(c("Loading", "Regression") %in% out_all$Component))
  expect_true(all(out_loading$Component == "Loading"))
  expect_setequal(unique(out_lr$Component), c("Loading", "Regression"))
})

test_that("model_estimates supports standardized variants and aliases", {
  skip_if_not_installed("lavaan")

  fit <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )

  out_raw <- suppressMessages(
    psymetrics::model_estimates(fit, standardized = FALSE, component = "regression", verbose = FALSE)
  )
  out_std_true <- suppressMessages(
    psymetrics::model_estimates(fit, standardized = TRUE, component = "regression", verbose = FALSE)
  )
  out_std_all <- suppressMessages(
    psymetrics::model_estimates(fit, standardized = "std.all", component = "regression", verbose = FALSE)
  )
  out_alias_all <- suppressMessages(
    psymetrics::model_estimates(fit, standardized = "all", component = "regression", verbose = FALSE)
  )
  out_legacy_partial <- suppressMessages(
    psymetrics::model_estimates(fit, standardize = "std.all", component = "regression", verbose = FALSE)
  )

  expect_false(isTRUE(all.equal(out_raw$Coefficient, out_std_true$Coefficient)))
  expect_equal(out_std_true$Coefficient, out_std_all$Coefficient)
  expect_equal(out_std_all$Coefficient, out_alias_all$Coefficient)
  expect_equal(out_std_all$Coefficient, out_legacy_partial$Coefficient)

  expect_silent(psymetrics::model_estimates(fit, standardized = "std.lv", verbose = FALSE))
  expect_silent(psymetrics::model_estimates(fit, standardized = "std.nox", verbose = FALSE))
  expect_silent(psymetrics::model_estimates(fit, standardized = "latent", verbose = FALSE))
  expect_silent(psymetrics::model_estimates(fit, standardized = "lv", verbose = FALSE))
  expect_silent(psymetrics::model_estimates(fit, standardized = "no_exogenous", verbose = FALSE))
})

test_that("lavaan_build_estimates_table emits non-duplicated missing inferential messages", {
  estimates_raw <- data.frame(
    lhs = "f",
    op = "=~",
    rhs = "x1",
    est = 0.7,
    ci.lower = 0.5,
    ci.upper = 0.9,
    stringsAsFactors = FALSE
  )

  msgs <- testthat::capture_messages(
    psymetrics:::lavaan_build_estimates_table(
      estimates_raw,
      coefficient_col = "est",
      verbose = TRUE
    )
  )

  expect_length(msgs, 1)
  expect_match(msgs[[1]], "were set to NA")
  expect_false(any(grepl("fully unavailable", msgs)))
})

test_that("model_estimates_compose_link keeps previous semantics", {
  to <- c("a", "b", "c", NA, NA, NA, " d ", "e")
  operator <- c("=~", "~1", NA, "~~", "~1", NA, " ~ ", "=~")
  from <- c("x", "1", "z", "y", NA, NA, "  q", "")

  out <- psymetrics:::model_estimates_compose_link(to, operator, from)

  expect_identical(
    out,
    c("a =~ x", "b ~1", NA, "NA ~~ y", "NA ~1", NA, "d ~ q", "e =~")
  )
})

test_that("model_estimates maps operators to components including defined terms", {
  skip_if_not_installed("lavaan")

  fit <- suppressWarnings(
    lavaan::sem(hs_sem_defined_model, data = lavaan::HolzingerSwineford1939)
  )
  out <- suppressMessages(psymetrics::model_estimates(fit, verbose = FALSE))

  is_variance <- out$Operator == "~~" & out$To == out$From
  is_correlation <- out$Operator == "~~" & out$To != out$From

  expect_true(any(is_variance & out$Component == "Variance"))
  expect_true(any(is_correlation & out$Component == "Correlation"))
  expect_true(any(out$Operator == ":=" & out$Component == "Defined"))
})

test_that("model_estimates captures thresholds and keeps unmapped operators as Other", {
  skip_if_not_installed("lavaan")

  hs_ord <- lavaan::HolzingerSwineford1939
  ordered_vars <- paste0("x", 1:9)
  hs_ord[ordered_vars] <- lapply(hs_ord[ordered_vars], function(x) {
    cut(x, breaks = 3, include.lowest = TRUE, ordered_result = TRUE)
  })

  fit <- suppressWarnings(
    lavaan::sem(
      hs_sem_model,
      data = hs_ord,
      estimator = "WLSMV",
      ordered = ordered_vars
    )
  )
  out <- suppressMessages(psymetrics::model_estimates(fit, verbose = FALSE))

  expect_true(any(out$Operator == "|" & out$Component == "Threshold"))
  expect_true(any(out$Component == "Other"))
})

test_that("model_estimates returns stable output for non-converged models", {
  skip_if_not_installed("lavaan")

  fit_bad <- suppressWarnings(
    lavaan::sem(
      hs_sem_model,
      data = lavaan::HolzingerSwineford1939[1:20, ],
      estimator = "MLR",
      control = list(iter.max = 1)
    )
  )
  if (isTRUE(lavaan::lavInspect(fit_bad, "converged"))) {
    skip("Model converged unexpectedly with iter.max = 1.")
  }

  out <- suppressMessages(psymetrics::model_estimates(fit_bad, verbose = FALSE))
  expected_raw <- lavaan::parameterEstimates(
    fit_bad,
    ci = TRUE,
    level = 0.95,
    standardized = FALSE,
    remove.eq = FALSE,
    remove.ineq = FALSE,
    remove.system.eq = FALSE
  )

  expect_s3_class(out, "model_estimates")
  expect_equal(nrow(out), nrow(expected_raw))
  expect_true(all(out$converged == FALSE))
  expect_true(any(!is.na(out$Coefficient)))
  expect_true(any(out$Component %in% c("Loading", "Regression", "Variance")))
  expect_true(any(is.na(out$SE)))
})

test_that("model_estimates keeps standardized estimates for non-converged models", {
  skip_if_not_installed("lavaan")

  fit_bad <- suppressWarnings(
    lavaan::sem(
      hs_sem_model,
      data = lavaan::HolzingerSwineford1939[1:20, ],
      estimator = "MLR",
      control = list(iter.max = 1)
    )
  )
  if (isTRUE(lavaan::lavInspect(fit_bad, "converged"))) {
    skip("Model converged unexpectedly with iter.max = 1.")
  }

  out_std <- suppressMessages(
    suppressWarnings(
      psymetrics::model_estimates(fit_bad, standardized = TRUE, verbose = FALSE)
    )
  )
  expected_std <- suppressWarnings(
    lavaan::standardizedSolution(
      fit_bad,
      type = "std.all",
      ci = TRUE,
      level = 0.95,
      remove.eq = FALSE,
      remove.ineq = FALSE,
      remove.def = FALSE
    )
  )

  expect_s3_class(out_std, "model_estimates")
  expect_equal(nrow(out_std), nrow(expected_std))
  expect_true(all(out_std$converged == FALSE))
  expect_equal(out_std$To, as.character(expected_std$lhs))
  expect_equal(out_std$Operator, as.character(expected_std$op))
  expect_equal(out_std$From, as.character(expected_std$rhs))
  expect_equal(out_std$Coefficient, as.numeric(expected_std$est.std))
})

test_that("model_estimates verbose messaging is controlled and lavaan warnings are preserved", {
  skip_if_not_installed("lavaan")

  fit_ok <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )
  msg_ok <- testthat::capture_messages(
    psymetrics::model_estimates(fit_ok, standardized = FALSE, verbose = TRUE)
  )
  expect_false(any(grepl("did not converge", msg_ok, ignore.case = TRUE)))

  fit_bad <- suppressWarnings(
    lavaan::sem(
      hs_sem_model,
      data = lavaan::HolzingerSwineford1939[1:20, ],
      estimator = "MLR",
      control = list(iter.max = 1)
    )
  )
  if (isTRUE(lavaan::lavInspect(fit_bad, "converged"))) {
    skip("Model converged unexpectedly with iter.max = 1.")
  }

  msg_verbose <- testthat::capture_messages(
    suppressWarnings(
      psymetrics::model_estimates(fit_bad, standardized = FALSE, verbose = TRUE)
    )
  )
  expect_true(any(grepl("did not converge", msg_verbose, ignore.case = TRUE)))
  expect_true(any(grepl("inferential columns are fully unavailable", msg_verbose, ignore.case = TRUE)))

  msg_silent <- testthat::capture_messages(
    suppressWarnings(
      psymetrics::model_estimates(fit_bad, standardized = FALSE, verbose = FALSE)
    )
  )
  expect_length(msg_silent, 0L)

  expected_warn <- testthat::capture_warnings(
    lavaan::standardizedSolution(
      fit_bad,
      type = "std.all",
      ci = TRUE,
      level = 0.95,
      remove.eq = FALSE,
      remove.ineq = FALSE,
      remove.def = FALSE
    )
  )
  out_warn <- testthat::capture_warnings(
    suppressMessages(
      psymetrics::model_estimates(fit_bad, standardized = TRUE, verbose = FALSE)
    )
  )

  normalize_warning <- function(x) {
    gsub("\\s+", " ", trimws(as.character(x)))
  }

  if (length(expected_warn) == 0L) {
    expect_length(out_warn, 0L)
  } else {
    expect_true(length(out_warn) >= 1L)
    expected_snippet <- substr(normalize_warning(expected_warn[[1]]), 1, 60)
    out_norm <- vapply(out_warn, normalize_warning, character(1))
    expect_true(any(grepl(expected_snippet, out_norm, fixed = TRUE)))
  }
})

test_that("model_estimates handles valid empty component filters across outputs", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("knitr")

  fit <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  out <- suppressMessages(
    psymetrics::model_estimates(fit, component = "defined", verbose = FALSE)
  )

  expect_s3_class(out, "model_estimates")
  expect_equal(nrow(out), 0)

  text_out <- psymetrics::format_results(out, output = "text")
  md_out <- psymetrics::format_results(out, output = "markdown")
  html_out <- psymetrics::format_results(out, output = "html")

  expect_true(is.character(text_out))
  expect_match(text_out, "No Parameters")
  expect_s3_class(md_out, "knitr_kable")
  expect_s4_class(html_out, "tinytable")

  printed <- capture.output(result <- withVisible(print(out)))
  expect_false(result$visible)
  expect_identical(result$value, out)
  expect_true(any(grepl("No Parameters", printed)))
})

test_that("format_results works for model_estimates in text, markdown, and html", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("knitr")

  fit <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )
  out <- suppressMessages(
    psymetrics::model_estimates(
      fit,
      component = c("loading", "regression"),
      verbose = FALSE
    )
  )

  text_out <- psymetrics::format_results(out, output = "text")
  md_out <- psymetrics::format_results(out, output = "markdown")
  html_out <- psymetrics::format_results(out, output = "html")

  expect_true(is.character(text_out))
  expect_match(text_out, "# Loading")
  expect_match(text_out, "# Regression")
  expect_s3_class(md_out, "knitr_kable")
  expect_s4_class(html_out, "tinytable")

  html_lines <- capture.output(print(html_out))
  expect_true(any(grepl("Loading", html_lines)))
  expect_true(any(grepl("Regression", html_lines)))
})

test_that("print.model_estimates prints text and returns object invisibly", {
  skip_if_not_installed("lavaan")

  fit <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  out <- suppressMessages(psymetrics::model_estimates(fit, verbose = FALSE))

  printed <- capture.output(result <- withVisible(print(out)))

  expect_false(result$visible)
  expect_identical(result$value, out)
  expect_s3_class(result$value, "model_estimates")
  expect_true(length(printed) > 0)
  expect_true(any(nzchar(printed)))
})

test_that("save_table supports model_estimates docx export", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  fit <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )
  out <- suppressMessages(
    psymetrics::model_estimates(
      fit,
      component = c("loading", "regression"),
      verbose = FALSE
    )
  )
  output_path <- tempfile(fileext = ".docx")

  result <- suppressMessages(
    psymetrics::save_table(out, path = output_path, orientation = "portrait")
  )

  expect_true(file.exists(output_path))
  expect_equal(result, output_path)

  xml_conn <- unz(output_path, "word/document.xml")
  on.exit(close(xml_conn), add = TRUE)
  xml_text <- paste(readLines(xml_conn, warn = FALSE), collapse = "")

  expect_match(xml_text, "Loading")
  expect_match(xml_text, "Regression")
})

test_that("save_table supports empty model_estimates components", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  fit <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  out <- suppressMessages(
    psymetrics::model_estimates(fit, component = "defined", verbose = FALSE)
  )
  output_path <- tempfile(fileext = ".docx")

  result <- suppressMessages(
    psymetrics::save_table(out, path = output_path, orientation = "portrait")
  )

  expect_true(file.exists(output_path))
  expect_equal(result, output_path)

  xml_conn <- unz(output_path, "word/document.xml")
  on.exit(close(xml_conn), add = TRUE)
  xml_text <- paste(readLines(xml_conn, warn = FALSE), collapse = "")

  expect_match(xml_text, "No Parameters")
})
