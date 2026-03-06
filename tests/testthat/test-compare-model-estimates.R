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


test_that("compare_model_estimates aligns shared and model-specific parameters", {
  skip_if_not_installed("lavaan")

  fit1 <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  fit2 <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )

  out <- suppressMessages(
    psymetrics::compare_model_estimates(CFA = fit1, SEM = fit2, verbose = FALSE)
  )

  expect_s3_class(out, "compare_model_estimates")
  expect_equal(attr(out, "model_names"), c("CFA", "SEM"))
  expect_true(all(c(
    "Group", "Level", "Component", "To", "Operator", "From",
    "Coefficient.CFA", "SE.CFA", "CI_low.CFA", "CI_high.CFA", "z.CFA", "p.CFA",
    "Coefficient.SEM", "SE.SEM", "CI_low.SEM", "CI_high.SEM", "z.SEM", "p.SEM"
  ) %in% names(out)))

  reg_row <- out[
    out$Component == "Regression" &
      out$To == "textual" &
      out$Operator == "~" &
      out$From == "visual",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(reg_row), 1)
  expect_true(is.na(reg_row$Coefficient.CFA))
  expect_false(is.na(reg_row$Coefficient.SEM))
})


test_that("compare_model_estimates supports named and unnamed model labels", {
  skip_if_not_installed("lavaan")

  fit1 <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  fit2 <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )

  out <- suppressMessages(
    psymetrics::compare_model_estimates(CFA = fit1, fit2, verbose = FALSE)
  )
  blocks <- psymetrics:::prepare_table.compare_model_estimates(out)

  expect_equal(attr(out, "model_names"), c("CFA", "fit2"))
  expect_true(all(c("CFA", "fit2") %in% names(blocks[[1]])))
})


test_that("compare_model_estimates protects display columns from label collisions", {
  skip_if_not_installed("lavaan")

  fit1 <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  fit2 <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )

  out <- suppressMessages(
    psymetrics::compare_model_estimates(Link = fit1, `p (Link)` = fit2, verbose = FALSE)
  )
  blocks <- psymetrics:::prepare_table.compare_model_estimates(out, select = "ci_p2")
  block_names <- names(blocks[[1]])

  expect_equal(anyDuplicated(block_names), 0L)
  expect_equal(sum(block_names == "Link"), 1L)
  expect_true("estimate (Link)" %in% block_names)
  expect_true("p (Link)" %in% block_names)
  expect_true("estimate (p (Link))" %in% block_names)
})


test_that("compare_model_estimates propagates ci, component, and standardized settings", {
  skip_if_not_installed("lavaan")

  fit1 <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )
  fit2 <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )

  out <- suppressMessages(
    psymetrics::compare_model_estimates(
      fit1,
      fit2,
      standardized = "std.lv",
      ci = 0.90,
      component = "regression",
      verbose = FALSE
    )
  )
  single <- suppressMessages(
    psymetrics::model_estimates(
      fit1,
      standardized = "std.lv",
      ci = 0.90,
      component = "regression",
      verbose = FALSE
    )
  )

  expect_true(all(out$Component == "Regression"))
  expect_equal(attr(out, "ci"), 0.90)
  expect_equal(attr(out, "standardized"), "std.lv")

  match_idx <- match(
    paste(out$To, out$Operator, out$From),
    paste(single$To, single$Operator, single$From)
  )
  expect_equal(out$Coefficient.fit1, single$Coefficient[match_idx])
})


test_that("compare_model_estimates keeps group identifiers for multigroup models", {
  skip_if_not_installed("lavaan")

  fit1 <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939, group = "school")
  )
  fit2 <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939, group = "school")
  )

  out <- suppressMessages(
    psymetrics::compare_model_estimates(fit1, fit2, component = c("loading", "regression"), verbose = FALSE)
  )
  blocks <- psymetrics:::prepare_table.compare_model_estimates(out)

  expect_true("Group" %in% names(out))
  expect_true(any(!is.na(out$Group)))
  expect_true(all(vapply(blocks, function(block) "Group" %in% names(block), logical(1))))
})


test_that("compare_model_estimates prepare_table supports presets and custom templates", {
  skip_if_not_installed("lavaan")

  fit1 <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  fit2 <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )

  out <- suppressMessages(
    psymetrics::compare_model_estimates(CFA = fit1, SEM = fit2, verbose = FALSE)
  )

  preset_blocks <- psymetrics:::prepare_table.compare_model_estimates(out, select = "ci_p2")
  custom_blocks <- psymetrics:::prepare_table.compare_model_estimates(
    out,
    select = "{estimate}{stars} ({se})"
  )

  expect_true(all(c("CFA", "p (CFA)", "SEM", "p (SEM)") %in% names(preset_blocks[[1]])))
  expect_true(any(grepl("*", custom_blocks[[1]]$CFA, fixed = TRUE)))
})


test_that("compare_model_estimates validates select templates", {
  skip_if_not_installed("lavaan")

  fit1 <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  fit2 <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )

  out <- suppressMessages(
    psymetrics::compare_model_estimates(fit1, fit2, verbose = FALSE)
  )

  expect_error(
    psymetrics:::prepare_table.compare_model_estimates(out, select = "{estimate}|{p}|{se}"),
    "at most one `\\|`"
  )
  expect_error(
    psymetrics:::prepare_table.compare_model_estimates(out, select = "{estimate} {foo}"),
    "Unknown tokens"
  )
})


test_that("compare_model_estimates handles non-converged models and verbose control", {
  skip_if_not_installed("lavaan")

  fit_ok <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  )
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
    suppressWarnings(psymetrics::compare_model_estimates(fit_ok, bad = fit_bad, verbose = TRUE))
  )
  msg_silent <- testthat::capture_messages(
    suppressWarnings(psymetrics::compare_model_estimates(fit_ok, bad = fit_bad, verbose = FALSE))
  )
  out <- suppressMessages(
    suppressWarnings(psymetrics::compare_model_estimates(fit_ok, bad = fit_bad, verbose = FALSE))
  )

  expect_true(any(grepl("did not converge", msg_verbose, ignore.case = TRUE)))
  expect_length(msg_silent, 0L)
  expect_true(all(!out$converged.bad[!is.na(out$converged.bad)]))
})


test_that("compare_model_estimates supports valid empty comparisons across outputs", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("knitr")

  fit1 <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  fit2 <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )

  out <- suppressMessages(
    psymetrics::compare_model_estimates(fit1, fit2, component = "defined", verbose = FALSE)
  )

  expect_s3_class(out, "compare_model_estimates")
  expect_equal(nrow(out), 0)
  expect_match(psymetrics::format_results(out, output = "text"), "No Parameters")
  expect_s3_class(psymetrics::format_results(out, output = "markdown"), "knitr_kable")
  expect_s4_class(psymetrics::format_results(out, output = "html"), "tinytable")
})


test_that("compare_model_estimates format_results and print work across outputs", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("knitr")

  fit1 <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  fit2 <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )

  out <- suppressMessages(
    psymetrics::compare_model_estimates(CFA = fit1, SEM = fit2, verbose = FALSE)

  )

  text_out <- psymetrics::format_results(out, output = "text", table_args = list(select = "ci_p2"))
  md_out <- psymetrics::format_results(out, output = "markdown")
  html_out <- psymetrics::format_results(out, output = "html")
  printed <- capture.output(result <- withVisible(print(out)))

  expect_match(text_out, "# Loading")
  expect_match(text_out, "p \\(CFA\\)")
  expect_s3_class(md_out, "knitr_kable")
  expect_s4_class(html_out, "tinytable")
  expect_false(result$visible)
  expect_identical(result$value, out)
  expect_true(any(nzchar(printed)))
})


test_that("compare_model_estimates save_table supports docx export", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  fit1 <- suppressWarnings(
    lavaan::cfa(hs_cfa_model, data = lavaan::HolzingerSwineford1939)
  )
  fit2 <- suppressWarnings(
    lavaan::sem(hs_sem_model, data = lavaan::HolzingerSwineford1939)
  )

  out <- suppressMessages(
    psymetrics::compare_model_estimates(CFA = fit1, SEM = fit2, verbose = FALSE)
  )
  output_path <- tempfile(fileext = ".docx")

  result <- suppressMessages(
    psymetrics::save_table(out, path = output_path, orientation = "portrait", table_args = list(select = "ci_p2"))
  )

  expect_true(file.exists(output_path))
  expect_equal(result, output_path)

  xml_conn <- unz(output_path, "word/document.xml")
  on.exit(close(xml_conn), add = TRUE)
  xml_text <- paste(readLines(xml_conn, warn = FALSE), collapse = "")

  expect_match(xml_text, "Loading")
  expect_match(xml_text, "Regression")
  expect_match(xml_text, "CFA")
  expect_match(xml_text, "SEM")
})


