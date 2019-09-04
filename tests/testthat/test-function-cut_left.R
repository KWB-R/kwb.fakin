test_that("cut_left() works", {

  f <- kwb.fakin:::cut_left

  expect_error(f())

  expect_identical(f("abcdef", start_string = "abc"), "def")
  expect_error(f(c("abc", "bcd"), start_string = "a"))
})
