test_that("plot_treemaps_from_path_data() works", {

  f <- kwb.fakin:::plot_treemaps_from_path_data

  expect_error(f())

  path_data <- kwb.utils::noFactorDataFrame(
    path = c("a/b/c", "a/b/d", "a/b/e"),
    size = c(1, 2, 3),
    type = "file"
  )

  expect_error(f(path_data, n_levels = 1))
})
