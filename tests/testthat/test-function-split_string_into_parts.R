test_that("split_string_into_parts() works", {

  f <- kwb.fakin:::split_string_into_parts

  expect_error(f())

  expect_length(f("lajfljaslfjaljfaljf", size = 10), 2)
  expect_length(f("lajfljaslfjaljfaljf", size = 30), 1)
})

