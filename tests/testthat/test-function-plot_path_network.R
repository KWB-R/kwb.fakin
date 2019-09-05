test_that("plot_path_network() works", {

  f <- kwb.fakin:::plot_path_network

  expect_error(f())

  paths <- c("a/b/c", "a/b/d", "a/b/e/f")

  f(paths, method = 1)
  f(paths, method = 2)

  f(paths, names_to_colours = NULL, height = "200px")
})
