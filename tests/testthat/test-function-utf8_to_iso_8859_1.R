test_that("utf8_to_iso_8859_1() works", {

  f <- kwb.fakin:::utf8_to_iso_8859_1

  expect_error(f())

  expect_identical(f("a"), "a")
})
