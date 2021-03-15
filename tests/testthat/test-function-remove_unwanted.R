test_that("remove_unwanted() works", {

  f <- kwb.fakin:::remove_unwanted

  expect_error(f())
  expect_identical(f("a"), "a")
})
