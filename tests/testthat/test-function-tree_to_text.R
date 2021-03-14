test_that("tree_to_text() works", {

  f <- kwb.fakin:::tree_to_text

  expect_error(f())

  expect_null(f(list()))

  capture.output(tree <- kwb.fakin:::to_tree(c("a/b", "a/b/c", "a/b/c/d")))

  f(tree)
})
