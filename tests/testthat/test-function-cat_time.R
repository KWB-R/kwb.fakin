library(testthat)

test_that("cat_time() works", {

  expect_error(kwb.fakin:::cat_time())

  expect_output(kwb.fakin:::cat_time("tag"), "tag:")
})
