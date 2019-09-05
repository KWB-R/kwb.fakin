test_that("plot_file_distribution() works", {

  f <- kwb.fakin:::plot_file_distribution

  expect_error(f())

  file_data <- kwb.utils::noFactorDataFrame(
    path = "a/b/c", size = 1, type = "file"
  )

  f(file_data, start_path = "a", n_root_parts = 1, to_pdf = FALSE)
})
