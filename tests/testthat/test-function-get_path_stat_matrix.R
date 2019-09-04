test_that("get_path_stat_matrix() works", {

  f <- kwb.fakin:::get_path_stat_matrix

  expect_error(f())

  f(project_folder = file.path(package = "kwb.fakin"))
})
