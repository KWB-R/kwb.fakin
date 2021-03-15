test_that("getProjectPaths() works", {

  f <- kwb.fakin:::getProjectPaths

  expect_error(f())
  expect_length(f("start/path"), 0)
  expect_length(f("start/path", as_list = TRUE), 0)
})

