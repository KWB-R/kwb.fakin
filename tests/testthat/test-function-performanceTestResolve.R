test_that("performanceTestResolve() works", {

  f <- kwb.fakin:::performanceTestResolve

  expect_error(f())

  expect_identical(dim(f("a", list(a = "b"), N = c(1, 100))), c(6L, 2L))
})

