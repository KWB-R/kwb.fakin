test_that("example_string_with_specials() works", {

  f <- kwb.fakin:::example_string_with_specials

  expect_error(f())
  expect_error(f("en"))
  expect_error(f("fr"))
  expect_is(f("de"), "character")
})
