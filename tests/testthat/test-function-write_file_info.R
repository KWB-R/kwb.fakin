test_that("write_file_info() works", {

  f <- kwb.fakin:::write_file_info

  expect_error(f())

  f(data.frame(path = "/a/b/c"), (file <- tempfile()))
  expect_true(file.exists(file))
})
