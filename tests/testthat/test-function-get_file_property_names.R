test_that("get_file_property_names() works", {

  f <- kwb.fakin:::get_file_property_names

  expect_is(f(), "character")
  expect_true(all(grepl("System", f())))
})
