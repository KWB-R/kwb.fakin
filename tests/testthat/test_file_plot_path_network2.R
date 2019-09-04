test_that("functions defined in plot_path_network2.R work", {

  paths <- c("a", "a/b", "a/b/c")
  prepare_paths_for_network2(paths)

  get_path_network2(paths, reverse = FALSE)
  get_path_network2(paths, reverse = TRUE)

  pl <- pathlist::pathlist(paths)

  get_path_network2(pl, reverse = FALSE)
  get_path_network2(pl, reverse = TRUE)

  get_links_at_depth2(2, pl)
  get_links_at_depth2(2, folder_data = data.frame(
    path = c("a", "a/b"),
    size = c(1, 2)
  ))
})
