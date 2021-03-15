test_that("toLongPath() works", {

  f <- kwb.fakin:::toLongPath

  expect_error(f())
  expect_identical(f("a", list(a = "b")), "b")
})
