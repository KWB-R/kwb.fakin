test_that("read_file_info_() works", {

  f <- kwb.fakin:::read_file_info

  expect_warning(expect_error(f()))

  file <- kwb.fakin:::extdata_file("example_file_info_1.csv")

  expect_warning(f(file))
})
