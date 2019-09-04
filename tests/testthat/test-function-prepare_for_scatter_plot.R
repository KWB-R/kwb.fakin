test_that("prepare_for_scatter_plot() works", {

  f <- kwb.fakin:::prepare_for_scatter_plot

  expect_error(f())

  file_data <- kwb.utils::noFactorDataFrame(
    path = c("a", "b"),
    size = c(1, 2),
    type = c("file", "file")
  )

  f(file_data = file_data, n_root_parts = 1)
  f(file_data = file_data, n_root_parts = 1, min_depth = 1)
  f(file_data = file_data, n_root_parts = 1, start_path = "a")
})
