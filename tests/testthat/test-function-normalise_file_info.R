test_that("normalise_file_info() works", {

  f <- kwb.fakin:::normalise_file_info

  expect_error(f())

  f(data.frame(size = 1))
  f(data.frame(size = 1, FullName = "a", Attributes = "Directory"))
})
