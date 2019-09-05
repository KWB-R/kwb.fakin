test_that("read_csv() works", {

  f <- kwb.fakin:::read_csv

  expect_error(f())

  file <- kwb.fakin:::extdata_file("example_file_info_1.csv")
  f(file, version = 1)
  f(file, version = 2)
  expect_error(f(file, version = 3))
})
