test_that("restoreEncoding() works", {

  f <- kwb.fakin:::restoreEncoding

  expect_error(f())

  expect_identical(f("hi", paste, "fans"), "hi fans")
})
