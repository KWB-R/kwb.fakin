test_that("store() works", {

  f <- kwb.fakin:::store

  xyz <- 1
  capture.output(result <- f(xyz, "my-script"))
  expect_identical(result, NULL)
})
