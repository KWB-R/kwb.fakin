test_that("get_depths_from_subdir_matrix() works", {

  f <- kwb.fakin:::get_depths_from_subdir_matrix

  expect_error(f())

  subdir_matrix <- matrix(c("a", "b", "c", "a", "b", NA), nrow = 2)

  expect_identical(f(subdir_matrix), c(3, 2))
})
