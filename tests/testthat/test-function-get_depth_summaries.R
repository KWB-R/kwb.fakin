test_that("get_depth_summaries() works", {

  f <- kwb.fakin:::get_depth_summaries

  expect_error(f())

  file_data <- kwb.utils::noFactorDataFrame(
    path = "a",
    size = 1,
    type = "file"
  )

  capture.output(result <- f(file_data, project_dir = "a"))
})
