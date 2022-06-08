test_that("check_depth_summaries() works", {

  f <- kwb.fakin:::check_depth_summaries

  expect_error(f())

  depth_summaries <- list(
    data.frame(
      n_files = c(10, 20),
      total_size = c(100, 200)
    ),
    data.frame(
      n_files = c(20, 10),
      total_size = c(200, 100)
    )
  )

  f(depth_summaries)
})
