test_that("get_example_read_and_write_matrices() works", {

  f <- kwb.fakin:::get_example_read_and_write_matrices

  expect_is(f(), "list")

})
