test_that("plot_file_size_in_depth() works", {

  f <- kwb.fakin:::plot_file_size_in_depth

  expect_error(f())

  df <- data.frame(extension = "xls", depth = 1, root = "a", size = c(1,2))

  f(df)
})

