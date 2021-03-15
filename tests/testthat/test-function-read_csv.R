test_that("read_csv() works", {

  f <- kwb.fakin:::read_csv

  expect_error(f())

  file <- fakin.path.app:::extdata_file("example_file_info_1.csv")

  capture.output(f(file, version = 1))
  capture.output(f(file, version = 2))

  expect_error(f(file, version = 3))
})
