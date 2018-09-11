#devtools::install_github("kwb-r/kwb.fakin")

library("kwb.utils")

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  Sys.setlocale(locale = "C")

  #path_repo <- "//medusa/processing/CONTENTS/folders_projects"
  path_repo <- "~/Desktop/Data/FAKIN/folders_projects"

  files <- dir(path_repo, full.names = TRUE)

  (file <- files[1])

  paths <- kwb.fakin::read_paths(file, do_sort = FALSE)

  paths_relative <- kwb.fakin::removeCommonRoot(paths)

  pattern_aquanes <- "^WWT_Department/Projects/AquaNES"

  paths_aquanes <- grep(
    pattern_aquanes, paths_relative, ignore.case = TRUE, value = TRUE
  )

  paths_aquanes_relative <- kwb.fakin::removeCommonRoot(x = paths_aquanes)

  catLines(paths_aquanes_relative)

  target_root <- file.path(desktop(), "test_diff", "aquanes")

  # Delete all existing directories!
  unlink(target_root, recursive = TRUE)

  write_paths_to_folder_tree(
    paths = paths_aquanes_relative,
    target_dir = createDirectory(target_root),
    max_depth = 3
  )

  paths_reread <- dir(target_root, include.dirs = TRUE, recursive = TRUE, full.names = TRUE)

  kwb.fakin:::plot_path_network(paths_reread, max_depth = 5)
}

# write_paths_to_folder_tree ---------------------------------------------------
write_paths_to_folder_tree <- function(paths, target_dir, max_depth = 2, depth = 0)
{
  #paths <- paths_aquanes_relative
  #target_dir <- target_root

  paths <- kwb.fakin:::remove_empty(paths, dbg = TRUE)

  if (length(paths) == 0) {

    return()
  }

  folder_matrix <- kwb.fakin:::toSubdirMatrix(paths)

  (top_level_folders <- folder_matrix[, 1])

  # Split the folder matrix into sub-matrices each of which refers to one
  # first-level folder
  subsets <- split(
    kwb.utils::asNoFactorDataFrame(folder_matrix), top_level_folders
  )

  # Remove the first level folders and convert the folder data frames back to
  # path vectors
  path_vectors <- lapply(subsets, function(x) {

    data_frame_to_paths(x[, -1, drop = FALSE])
  })

  # Loop through the top level folders
  for (top_level_folder in names(path_vectors)) {

    #top_level_folder <- names(path_vectors)[2]

    # If we are already at maximal depth, write one file per top level folder
    # containing the paths to all subfolders in each top level folder
    if (depth == max_depth) {

      write_paths_file(path_vectors, top_level_folder, output_dir = target_dir)

    } else {

      # Create the top level folder and call this function recursively
      new_target_dir <- file.path(target_dir, top_level_folder)

      kwb.utils::createDirectory(new_target_dir)

      write_paths_to_folder_tree(
        paths = path_vectors[[top_level_folder]],
        target_dir = new_target_dir,
        max_depth = max_depth,
        depth = depth + 1
      )
    }
  }
}

# data_frame_to_paths ----------------------------------------------------------
data_frame_to_paths <- function(df)
{
  stopifnot(is.data.frame(df))

  paths_with_trailing_slashes <- kwb.utils::pasteColumns(df, sep = "/")

  kwb.fakin:::remove_empty(gsub("/+$", "", paths_with_trailing_slashes))
}

# write_paths_file -------------------------------------------------------------
write_paths_file <- function(path_list, element, output_dir = "")
{
  paths <- kwb.utils::selectElements(path_list, element)
  file <- sprintf("%04d_FOLDERS_IN_%s.txt", length(paths), element)
  writeText(paths, file.path(output_dir, file))
}
