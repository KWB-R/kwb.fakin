test_that("filter_for_package_functions() works", {

  f <- kwb.fakin:::filter_for_package_functions

  result <- f(
    frequency_data = data.frame(
      script = "x",
      name = "sort",
      count = 2
    ),
    package = "base"
  )

  expect_identical(nrow(result), 1L)
})
