test_that("summary.path_tree() works", {

  f <- kwb.fakin:::summary.path_tree

  expect_error(f())

  capture.output(tree <- kwb.fakin:::to_tree(c("a/b", "a/b/c")))

  f(tree)
})
