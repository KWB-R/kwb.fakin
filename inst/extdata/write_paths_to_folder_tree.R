#devtools::install_github("kwb-r/kwb.fakin")

library("kwb.utils")

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  files <- dir("//medusa/processing/CONTENTS/folders_projects", full.names = TRUE)

  (file <- files[1])

  paths <- kwb.fakin::read_paths(file, do_sort = FALSE)

  paths_relative <- kwb.fakin::removeCommonRoot(paths)

  pattern_aquanes <- "^WWT_Department/Projects/AquaNES"

  paths_aquanes <- grep(
    pattern_aquanes, paths_relative, ignore.case = TRUE, value = TRUE
  )

  paths_aquanes_relative <- unlist(kwb.fakin::removeCommonRoot(paths_aquanes))

  catLines(paths_aquanes_relative)

  target_root <- file.path(desktop(), "test_diff", "aquanes")

  # Delete all existing directories!
  unlink(target_root, recursive = TRUE)

  write_paths_to_folder_tree(
    paths = paths_aquanes_relative,
    target_dir = createDirectory(target_root),
    max_depth = 6
  )
}

# write_paths_to_folder_tree ---------------------------------------------------
write_paths_to_folder_tree <- function(paths, target_dir, max_depth = 2, depth = 0)
{
  #paths <- paths_aquanes_relative
  #target_dir <- target_root

  if (depth == max_depth || length(paths) == 0 || all(paths == "")) {

    # Write a file containing all paths
    write_paths_file(paths, output_dir = target_dir)

  } else {

    folder_matrix <- kwb.fakin:::toSubdirMatrix(paths)

    #head(folder_matrix, 30)
    #View(folder_matrix)

    level_one_folders <- folder_matrix[, 1]

    # Create new directories
    target_paths <- file.path(target_dir, unique(level_one_folders))

    for (target_path in target_paths) {

      createDirectory(target_path)
    }

    # Split the folder matrix into sub-matrices each of which refers to one
    # first-level folder
    sub_matrices <- split(asNoFactorDataFrame(folder_matrix), level_one_folders)

    # Convert the folder matrices back to path vectors by removing the first
    # level folder
    path_vectors <- lapply(sub_matrices, function(x) {
      kwb.fakin:::remove_empty(gsub("/+$", "", pasteColumns(
        x, columns = names(x)[-1], sep = "/")
      ))
    })

    # Call this function recursively for each path vector
    for (i in seq_along(path_vectors)) {
      #i <- 1
      write_paths_to_folder_tree(
        paths = path_vectors[[i]],
        target_dir = target_paths[i],
        max_depth = max_depth,
        depth = depth + 1
      )
    }
  }
}

# write_paths_file -------------------------------------------------------------
write_paths_file <- function(paths, output_dir = "")
{
  #folder_matrix <- kwb.fakin:::toSubdirMatrix(paths)
  #file <- sprintf("FOLDERS_%s.txt", folder_matrix[1, 1])
  file <- sprintf("%04d_FOLDERS.txt", length(paths))
  writeText(paths, file.path(output_dir, file))
}
