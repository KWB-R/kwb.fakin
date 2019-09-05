test_that("filter_for_project() works", {

  f <- kwb.fakin:::filter_for_project

  expect_error(f())

  level_data <- data.frame(level_1 = "a")

  expect_identical(f(level_data, project = "a"), level_data)
  expect_error(f(level_data, project = "b"))
})
