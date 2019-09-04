test_that("store() works", {

  f <- kwb.fakin:::store

  expect_error(f())

  xyz <- 1
  f(xyz, "my-script")
  expect_message(f(xyz, "my-script"))
})
