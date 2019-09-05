test_that("cat_changes_if() works", {

  f <- kwb.fakin:::cat_changes_if

  expect_error(f())

  f(TRUE, 1:2, 2:3)
})
