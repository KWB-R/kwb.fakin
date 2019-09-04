test_that("plot_all_treemaps() works", {

  f <- kwb.fakin:::plot_all_treemaps

  expect_error(f())
  expect_error(f(1))
  expect_error(f(list(data.frame())))

  path_infos <- list(a = data.frame())

  f(path_infos)
})
