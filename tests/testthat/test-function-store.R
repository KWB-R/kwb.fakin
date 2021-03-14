test_that("store() works", {

  f <- kwb.fakin:::store

  expect_error(f())

  xyz <- 1
  capture.output(f(xyz, "my-script"))
})
