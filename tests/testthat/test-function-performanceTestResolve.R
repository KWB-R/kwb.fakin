test_that("performanceTestResolve() works", {

  f <- kwb.fakin:::performanceTestResolve

  capture.output(expect_error(f()))

  capture.output(result <- f("a", list(a = "b"), N = c(1, 100)))

  expect_identical(dim(result), c(6L, 2L))
})

