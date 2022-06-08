test_that("extdata_file() works", {

  f <- kwb.fakin:::extdata_file

  expect_error(f())
  expect_error(f("abc"))
  f("main.c")
})
