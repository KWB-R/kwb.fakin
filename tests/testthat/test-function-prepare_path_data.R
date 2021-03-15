test_that("prepare_path_data() works", {

  f <- kwb.fakin:::prepare_path_data

  expect_error(f())

  path_info <- data.frame(path = "a", size = 1, type = "file")

  capture.output(f(path_info))
  capture.output(f(path_info, pattern = "a"))
})
