# build_folders_from_file ------------------------------------------------------
build_folders_from_file <- function(
  file, target_dir, pattern = NULL, max_depth = NULL, encoding = "Latin-1"
)
{
  paths_raw <- kwb.fakin::read_paths(file, do_sort = FALSE, encoding = encoding)

  paths <- if (! is.null(pattern)) {

    grep(pattern, paths_raw, ignore.case = TRUE, value = TRUE)

  } else {

    paths_raw
  }

  paths <- kwb.fakin::removeCommonRoot(x = paths)

  max_depth <- kwb.utils::defaultIfNULL(max_depth, maxdepth(paths = paths))

  write_paths_to_folder_tree(paths, createDirectory(target_dir), max_depth)
}

# write_paths_to_folder_tree ---------------------------------------------------
write_paths_to_folder_tree <- function(
  paths, target_dir, max_depth = 2, depth = 0
)
{
  #paths <- paths_aquanes_relative
  #target_dir <- target_dir

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
  #file <- sprintf("%04d_FOLDERS_IN_%s.txt", length(paths), element)
  n_paths <- length(paths)

  file <- sprintf(
    "%s__WITH%s-SUBFOLDER%s.txt",
    element,
    if (n_paths > 0) paste0("-", n_paths) else "OUT",
    if (n_paths != 1) "S" else ""
  )

  writeText(paths, file.path(output_dir, file))
}
