test_that("bytes_to_mib() works", {

  f <- kwb.fakin:::bytes_to_mib

  expect_error(f())

  expect_identical(f(1024*1024), 1)
})

