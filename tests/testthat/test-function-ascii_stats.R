test_that("ascii_stats() works", {

  f <- kwb.fakin:::ascii_stats

  expect_error(f())

  result <- f(c("a", "b", "c"))
  expect_is(result, "table")
  expect_equal(as.numeric(result), 100)
})
