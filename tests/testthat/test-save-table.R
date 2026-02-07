test_that("save_table rejects unsupported formats", {
  table_data <- data.frame(MODEL = "fit", NOBS = 1)

  expect_error(
    psymetrics::save_table(table_data, path = "table.txt"),
    "Unsupported file format"
  )
})

test_that("save_table validates table_args and digits_by_col", {
  table_data <- data.frame(MODEL = "fit", NOBS = 1)
  output_path <- tempfile(fileext = ".docx")

  expect_error(
    psymetrics::save_table(table_data, path = output_path, table_args = "bad"),
    "`table_args` must be a list"
  )
  expect_error(
    psymetrics::save_table(table_data, path = output_path, table_args = list(1)),
    "`table_args` must use named entries"
  )
  expect_error(
    psymetrics::save_table(table_data, path = output_path, digits_by_col = c(2)),
    "`digits_by_col` must be a named numeric vector"
  )
  expect_error(
    psymetrics::save_table(table_data, path = output_path, digits_by_col = c(NOBS = 2.5)),
    "`digits_by_col` must contain whole-number digits"
  )
})

test_that("save_table writes docx when dependencies are available", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  table_data <- data.frame(MODEL = "fit", NOBS = 1)
  output_path <- tempfile(fileext = ".docx")

  result <- suppressMessages(
    psymetrics::save_table(table_data, path = output_path, orientation = "portrait")
  )

  expect_true(file.exists(output_path))
  expect_equal(result, output_path)

  xml_conn <- unz(output_path, "word/document.xml")
  on.exit(close(xml_conn), add = TRUE)
  xml_text <- paste(readLines(xml_conn, warn = FALSE), collapse = "")

  expect_match(xml_text, "MODEL")
  expect_match(xml_text, "fit")
})
