test_that("cat_time() works", {

  expect_error(kwb.fakin:::cat_time())

  kwb.fakin:::cat_time("tag")
})
