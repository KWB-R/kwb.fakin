test_that("flatten_tree() works", {

  f <- kwb.fakin:::flatten_tree

  expect_error(f())

  paths <- c("a/b", "a/c", "a/d/e/f")

  tree <- kwb.fakin:::to_tree(paths)

  expect_identical(f(paths), paths)
  expect_identical(f(tree), paths)
})
