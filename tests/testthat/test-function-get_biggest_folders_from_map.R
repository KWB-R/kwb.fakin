test_that("get_biggest_folders_from_map() works", {

  f <- kwb.fakin:::get_biggest_folders_from_map

  expect_error(f())

  f(list(tm = data.frame(level = 1, vSize = 1), level = 1), n = -1)
  f(list(tm = data.frame(level = 1, vSize = 1), level = 1), n = 1)

})
