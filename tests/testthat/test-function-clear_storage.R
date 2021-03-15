test_that("clear_storage() works", {

  f <- kwb.fakin:::clear_storage

  expect_error(f())
  expect_output(f("abc"), "No objects")

  xyz <- 1
  capture.output(kwb.fakin:::store(xyz, script = "testscript"))
  expect_output(f("xyz"), "Deleting")
})
