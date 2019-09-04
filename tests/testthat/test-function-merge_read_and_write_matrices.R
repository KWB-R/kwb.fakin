test_that("merge_read_and_write_matrices() works", {

  f <- kwb.fakin:::merge_read_and_write_matrices

  expect_error(f())
  expect_warning(f(matrix(), matrix()))

  m1 <- kwb.utils::createMatrix(c("a", "b", "c"), c("x", "y"))

  f(m1, m1)
})
