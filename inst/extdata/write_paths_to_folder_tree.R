#devtools::install_github("kwb-r/kwb.fakin")

library("kwb.utils")

Sys.setlocale(locale = "C")

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  #path_repo <- "//medusa/processing/CONTENTS/folders_projects"
  path_repo <- "~/Desktop/Data/FAKIN/folders_projects"

  files <- dir(path_repo, full.names = TRUE)

  # Set the target directory
  target_root <- file.path(desktop(), "test_diff")

  # Set a pattern to select paths of one project
  pattern <- "/WWT_Department/Projects/AquaNES"

  # Delete all existing directories below target_root!
  unlink(dir(target_root, all.files = TRUE, full.names = TRUE, recursive = TRUE))

  kwb.fakin:::build_folders_from_file(
    file = files[1],
    pattern = pattern,
    target_dir = file.path(target_root, "aquanes-1"),
    max_depth = 2
  )

  kwb.fakin:::build_folders_from_file(
    file = files[2],
    pattern = pattern,
    target_dir = file.path(target_root, "aquanes-2"),
    max_depth = 2
  )

  # Read back the paths to the created folder structure
  paths_reread <- dir(target_dir, include.dirs = TRUE, recursive = TRUE, full.names = TRUE, all.files = TRUE)

  # Make the paths comparable to the source paths
  paths_reread <- sort(kwb.fakin::removeCommonRoot(paths_reread))

  # Compare the original and the created paths
  head(paths_reread)
  head(sort(paths_aquanes_relative))

  # Which paths are missing?
  paths_reread[! paths_reread %in% paths_aquanes_relative]
  paths_aquanes_relative[! paths_aquanes_relative %in% paths_reread]
}
