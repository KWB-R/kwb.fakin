test_that("clear_storage() works", {

  f <- kwb.fakin:::clear_storage

  expect_error(f())
  f("abc")

  xyz <- 1
  kwb.fakin:::store(xyz, script = "testscript")
  f("xyz")
})
