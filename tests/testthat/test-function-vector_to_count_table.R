test_that("vector_to_count_table() works", {

  f <- kwb.fakin:::vector_to_count_table

  expect_error(f())

  expect_null(f(character()))

  f(c("x", "x", "y", "x", "y"))
})
