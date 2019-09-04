test_that("plot_biggest_folders() works", {

  f <- kwb.fakin:::plot_biggest_folders

  expect_error(f())

  paths <- dir(
    system.file(package = "kwb.fakin"), full.names = TRUE, recursive = TRUE
  )

  tree <- kwb.fakin:::to_tree(x = paths)

  f(tree, to_pdf = FALSE)
})

