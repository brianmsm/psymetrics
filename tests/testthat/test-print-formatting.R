test_that("format_results returns text output", {
  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  text_out <- format_results(table_data, output = "text")
  expect_true(is.character(text_out))
})

test_that("format_results validates table_args and output_args", {
  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  expect_error(
    format_results(table_data, table_args = "bad"),
    "`table_args` must be a list"
  )
  expect_error(
    format_results(table_data, table_args = list(1)),
    "`table_args` must use named entries"
  )
  expect_error(
    format_results(table_data, output_args = "bad"),
    "`output_args` must be a list"
  )
  expect_error(
    format_results(table_data, output_args = list(1)),
    "`output_args` must use named entries"
  )
})

test_that("format_results rejects overlapping digit arguments", {
  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  expect_error(
    format_results(table_data, digits = 2, table_args = list(digits = 1)),
    "Do not pass `digits/ci_digits/p_digits`"
  )
})

test_that("format_results validates digits_by_col inputs", {
  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  expect_error(
    format_results(table_data, digits_by_col = c(2)),
    "`digits_by_col` must be a named numeric vector"
  )
  expect_error(
    format_results(table_data, digits_by_col = c(Chi2 = 2.5)),
    "`digits_by_col` must contain whole-number digits"
  )
})

test_that("apply_digits_by_col formats numeric cells only", {
  table_data <- data.frame(Chi2 = c("1.234", "< .001", "2"), check.names = FALSE)

  result <- psymetrics:::apply_digits_by_col(
    table_data,
    digits_by_col = c(Chi2 = 1)
  )

  expect_equal(result$Chi2, c("1.2", "< .001", "2.0"))
})

test_that("apply_digits_by_col formats scientific notation values", {
  table_data <- data.frame(Chi2 = c("1e-05", "-2E+03", "< .001"), check.names = FALSE)

  result <- psymetrics:::apply_digits_by_col(
    table_data,
    digits_by_col = c(Chi2 = 2)
  )

  expect_equal(result$Chi2, c("0.00", "-2000.00", "< .001"))
})

test_that("format_results.data.frame applies digits_by_col", {
  table_data <- data.frame(Value = c(1.234, 2.5), check.names = FALSE)

  text_out <- format_results(
    table_data,
    output = "text",
    digits = 3,
    digits_by_col = c(Value = 1)
  )

  expect_match(text_out, "1.2")
  expect_match(text_out, "2.5")
})

test_that("format_results rounds Chi2 df labels to 2 decimals", {
  table_data <- data.frame(
    Chi2 = 20.68,
    Chi2_df = 7.33652992775491,
    p_Chi2 = 0.005,
    check.names = FALSE
  )
  class(table_data) <- c("model_fit", class(table_data))

  text_out <- format_results(table_data, output = "text")

  expect_match(text_out, "Chi2\\(7\\.34\\)")
})

test_that("format_results returns markdown output", {
  skip_if_not_installed("knitr")

  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  md_out <- format_results(table_data, output = "markdown")
  expect_s3_class(md_out, "knitr_kable")
})

test_that("format_results accepts md alias for markdown output", {
  skip_if_not_installed("knitr")

  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  md_out <- format_results(table_data, output = "md")
  expect_s3_class(md_out, "knitr_kable")
})

test_that("format_results returns html output", {
  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  html_out <- format_results(table_data, output = "html")
  expect_s4_class(html_out, "tinytable")
})

test_that("format_results auto defaults to markdown in non-interactive sessions", {
  skip_if_not_installed("knitr")

  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  old_target <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old_target), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = NULL)

  auto_out <- format_results(table_data, output = "auto")
  expect_s3_class(auto_out, "knitr_kable")
})

test_that("format_results auto returns html when knitr target is html", {
  skip_if_not_installed("knitr")

  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  old_target <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old_target), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")

  auto_out <- format_results(table_data, output = "auto")
  expect_s4_class(auto_out, "tinytable")
})

test_that("format_results supports align through output_args alias", {
  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  text_out <- format_results(
    table_data,
    output = "text",
    output_args = list(align = "center")
  )
  expect_true(is.character(text_out))
})

test_that("format_results errors when align is passed in two places", {
  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  expect_error(
    format_results(
      table_data,
      output = "text",
      align = "left",
      output_args = list(align = "center")
    ),
    "Do not pass `align` in both places"
  )
})

test_that("format_results errors when output_args includes format", {
  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  expect_error(
    format_results(
      table_data,
      output = "text",
      output_args = list(format = "markdown")
    ),
    "`output_args\\$format` is not supported"
  )
})

test_that("print.model_fit prints text and returns x invisibly", {
  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  output <- capture.output(result <- withVisible(print(table_data)))

  expect_false(result$visible)
  expect_identical(result$value, table_data)
  expect_s3_class(result$value, "model_fit")
  expect_true(length(output) > 0)
  expect_true(any(nzchar(output)))
})

test_that("print.compare_model_fit prints text and returns x invisibly", {
  table_data <- data.frame(MODEL = "fit", Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("compare_model_fit", class(table_data))

  output <- capture.output(result <- withVisible(print(table_data)))

  expect_false(result$visible)
  expect_identical(result$value, table_data)
  expect_s3_class(result$value, "compare_model_fit")
  expect_true(length(output) > 0)
  expect_true(any(nzchar(output)))
})
