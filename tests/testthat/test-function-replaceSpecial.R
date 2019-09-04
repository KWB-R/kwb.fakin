test_that("replaceSpecial() works", {

  f <- kwb.fakin:::replaceSpecial

  expect_error(f())
  expect_identical(f("abc"), "abc")
  expect_identical(f("abc", all = FALSE), "abc")
})

