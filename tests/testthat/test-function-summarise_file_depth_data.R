test_that("summarise_file_depth_data() works", {

  f <- kwb.fakin:::summarise_file_depth_data

  expect_error(f())

  f(data.frame(depth = 1, root = "root", size = 1))
})
