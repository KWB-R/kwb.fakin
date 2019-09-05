test_that("get_and_save_file_info() works", {

  f <- kwb.fakin:::get_and_save_file_info

  expect_error(f())

  tdir <- tempdir()

  f(tdir, tdir, check_dirs = FALSE)
  f(tdir, tdir, check_dirs = TRUE)
  expect_error(f(tdir, file.path(tdir, "not-existing"), check_dirs = TRUE))
})
