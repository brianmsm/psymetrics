test_that("save_table rejects unsupported formats", {
  table_data <- data.frame(MODEL = "fit", NOBS = 1)

  expect_error(
    psymetrics::save_table(table_data, path = "table.txt"),
    "Unsupported file format"
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
})
