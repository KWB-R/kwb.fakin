test_that("merge_read_and_write_matrices() works", {

  f <- kwb.fakin:::merge_read_and_write_matrices

  expect_error(f())

  # expect_warning(f(matrix_read = matrix(), matrix_write = matrix()))
  #
  # m1 <- kwb.utils::createMatrix(c("a", "b", "c"), c("x", "y"))
  #
  # f(matrix_read = m1, matrix_write = m1)
})
