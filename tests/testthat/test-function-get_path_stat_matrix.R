test_that("get_path_stat_matrix() works", {

  f <- kwb.fakin:::get_path_stat_matrix

  expect_error(f())

  project_folder <- system.file(package = "kwb.fakin")

  capture.output(f(project_folder, template_folders = c("a", "b")))
})
