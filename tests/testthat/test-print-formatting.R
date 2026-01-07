test_that("format_results returns text output", {
  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  text_out <- format_results(table_data, output = "text")
  expect_true(is.character(text_out))
})

test_that("format_results returns markdown output", {
  skip_if_not_installed("knitr")

  table_data <- data.frame(Chi2 = 1.234, check.names = FALSE)
  class(table_data) <- c("model_fit", class(table_data))

  md_out <- format_results(table_data, output = "markdown")
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
