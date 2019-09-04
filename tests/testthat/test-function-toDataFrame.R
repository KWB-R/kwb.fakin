test_that("toDataFrame() works", {

  f <- kwb.fakin:::toDataFrame

  expect_error(f())
  f(1)
  f(list(1))
})
