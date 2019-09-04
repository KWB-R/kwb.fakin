test_that("example_string_with_specials() works", {

  f <- kwb.fakin:::example_string_with_specials

  expect_error(f())
  expect_error(f("en"))
  expect_errorf("fr")
  expect_is(f("de"), "character")
})
