test_that("plot_number_of_elements_per_folder() works", {

  f <- kwb.fakin:::plot_number_of_elements_per_folder

  expect_error(f())

  x <- kwb.fakin:::to_tree(c("a/b/c", "a/b/d", "a/b/e/f"))

  f(x)
})
