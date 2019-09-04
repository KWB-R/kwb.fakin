test_that("read_file_paths() works", {

  f <- kwb.fakin:::read_file_paths

  expect_error(f())

  file_1 <- kwb.fakin:::extdata_file("example_file_paths.csv")

  result <- f(file_1)

  expect_is(result, "data.frame")
})

