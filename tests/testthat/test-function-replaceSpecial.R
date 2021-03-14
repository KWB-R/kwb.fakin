test_that("replaceSpecial() works", {

  f <- kwb.fakin:::replaceSpecial

  expect_error(f())
  expect_identical(f("abc", dbg = FALSE), "abc")
  expect_identical(f("abc", all = FALSE, dbg = FALSE), "abc")
})

