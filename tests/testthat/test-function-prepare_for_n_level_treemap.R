test_that("prepare_for_n_level_treemap() works", {

  #library(testthat)
  f <- kwb.fakin:::prepare_for_n_level_treemap
  f2 <- kwb.fakin:::prepare_for_n_level_treemap2

  expect_error(f())

  path_data <- kwb.utils::noFactorDataFrame(
    path = c("a", "a/b1", "a/b1/c1.txt", "a/b2", "a/b2/c2.txt", "a/b2/c/d.txt"),
    size = c( 0L,    0L,            10L,     0L,           20L,            15L),
    type = c("directory", "directory", "file", "directory", "file", "file")
  )

  path_list_data <- pathlist::pathlist(
    path_data$path,
    data = kwb.utils::removeColumns(path_data, "path")
  )

  result_1 <- f(path_data)
  result_2 <- f(path_data, n_levels = 2)

  expect_identical(
    names(result_1), c("level_1", "n_files", "total_size")
  )

  expect_identical(
    names(result_2), c("level_1", "level_2", "n_files", "total_size")
  )

  result_3 <- f2(path_list = path_list_data)
  expect_identical(result_1, result_3)

  # kwb.fakin:::plot_treemap(result_1)
  #
  # args <- kwb.fakin:::args_treemap(n_levels = 2)
  # kwb.utils::callWith(kwb.fakin:::plot_treemap, dtf = result_2, args)
})
