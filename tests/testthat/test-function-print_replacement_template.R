test_that("print_replacement_template() works", {

  f <- kwb.fakin:::print_replacement_template

  expect_error(f())

  expect_output(f(c("x", "y", "z")))
})
