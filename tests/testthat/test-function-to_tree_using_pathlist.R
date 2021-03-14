test_that("to_tree_using_pathlist() works", {

  f <- kwb.fakin:::to_tree_using_pathlist

  expect_error(f())

  paths <- c("a/b", "a/b/c", "a/d")

  capture.output(result_1 <- f(paths))
  capture.output(result_2 <- f(pathlist::pathlist(paths)))

  expect_identical(result_1, result_2)
})
