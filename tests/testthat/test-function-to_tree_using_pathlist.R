test_that("to_tree_using_pathlist() works", {

  f <- kwb.fakin:::to_tree_using_pathlist

  expect_error(f())

  paths <- c("a/b", "a/b/c", "a/d")

  expect_identical(f(paths), f(pathlist::pathlist(paths)))
})
