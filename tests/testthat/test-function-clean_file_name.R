test_that("clean_file_name() works", {

  f <- kwb.fakin:::clean_file_name

  expect_error(f())

  expect_identical(f("abc"), "abc")
})
