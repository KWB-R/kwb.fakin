test_that("get_names_recursively() works", {

  f <- kwb.fakin:::get_names_recursively

  expect_error(f())

  expect_null(f(1))
  expect_identical(f(list(a = list(b = 1))), c("a", "b"))
})
