test_that("extend_each_element() works", {

  f <- kwb.fakin:::extend_each_element

  expect_error(f())
  expect_error(f(1))

  f(list())
  f(list(list(), list()), a = 1, b = 2)
})
