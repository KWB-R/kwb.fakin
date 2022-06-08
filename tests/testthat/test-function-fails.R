test_that("fails() works", {

  f <- kwb.fakin:::fails

  expect_true(f(stop()))
  expect_false(f(x <- 1))
  expect_true(f(mean()))
})
