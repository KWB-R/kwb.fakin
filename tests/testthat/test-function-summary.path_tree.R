test_that("summary.path_tree() works", {

  f <- kwb.fakin:::summary.path_tree

  expect_error(f())

  f(kwb.fakin:::to_tree(c("a/b", "a/b/c")))
})
