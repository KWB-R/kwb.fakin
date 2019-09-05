test_that("build_folders_from_file() works", {

  # Create a vector of example paths
  paths <- c("a1/b1", "a1/b2", "a2/b1", "a2/b1/c1")

  # Write the example paths to a temporary file
  writeLines(paths, file <- tempfile())

  # Create a temporary target directory
  target_dir <- kwb.utils::createDirectory(file.path(tempdir(), "test"))

  # Create the folder structure as defined by the paths in the temporary file
  kwb.fakin::build_folders_from_file(file, target_dir)

  # List the directory paths below the target directory
  paths_reread <- list.dirs(target_dir, recursive = TRUE, full.names = FALSE)

  # Check that all paths have been created
  expect_true(all(paths %in% paths_reread))
})
