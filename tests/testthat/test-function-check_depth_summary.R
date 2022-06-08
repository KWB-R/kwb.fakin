test_that("check_depth_summary() works", {

  f <- kwb.fakin:::check_depth_summary

  expect_error(f())
  expect_error(f(1))
  expect_error(f(data.frame()))

  depth_summary <- data.frame(level_1 = 1, n_files = 2, total_size = 3)

  f(depth_summary)
})
