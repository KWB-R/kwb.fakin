test_that("extend_file_info() works", {

  root_dir <- system.file(package = "kwb.fakin")
  file_info <- fakin.path.app::get_recursive_file_info(root_dir, dbg = FALSE)

  f <- kwb.fakin:::extend_file_info

  expect_error(f())

  capture.output(file_info2 <- f(file_info))

  expect_true(all(c("id", "pathString") %in% names(file_info2)))
})
