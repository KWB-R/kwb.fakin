test_that("add_percentage_of_sum_columns() works", {

  f <- kwb.fakin:::add_percentage_of_sum_columns

  expect_error(f())

  result <- f(data.frame(a = 1:10), "a")

  expect_identical(names(result), c("a", "perc_a"))
})
