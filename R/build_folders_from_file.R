# build_folders_from_file ------------------------------------------------------

#' Create Folder Structure from Paths in File
#'
#' @param file path to file containing path strings
#' @param target_dir path to target directory in which to create the folder
#'   structure
#' @param pattern regular expression matching the paths from \code{file} to be
#'   considered
#' @param max_depth maximum folder depth to be considered
#' @param encoding encoding used when reading \code{file}
#'
#' @export
#'
#' @examples
#' # Create a vector of example paths
#' paths <- c("a1/b1", "a1/b2", "a2/b1", "a2/b1/c1")
#'
#' # Write the example paths to a temporary file
#' writeLines(paths, file_paths <- tempfile())
#'
#' # Create a temporary target directory
#' target_dir <- kwb.utils::createDirectory(file.path(tempdir(), "test"))
#'
#' # Create the folder structure as defined by the paths in the temporary file
#' kwb.fakin::build_folders_from_file(file_paths, target_dir)
#'
#' # List the directory paths below the target directory
#' paths_reread <- list.dirs(target_dir, recursive = TRUE, full.names = FALSE)
#'
#' # Stop if not all paths have been created
#' stopifnot(all(paths %in% paths_reread))
#'
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

  max_depth <- kwb.utils::defaultIfNULL(
    max_depth, kwb.file:::get_max_path_depth(paths = paths)
  )

  kwb.utils::createDirectory(target_dir)

  write_paths_to_folder_tree(paths, target_dir, max_depth)
}

# write_paths_to_folder_tree ---------------------------------------------------
write_paths_to_folder_tree <- function(
  paths, target_dir, max_depth = 2, depth = 0
)
{
  paths <- remove_empty(paths, dbg = TRUE)

  if (length(paths) == 0) {

    return()
  }

  folder_data <- toSubdirMatrix(paths, result_type = "data.frame")

  # Split the folder matrix into sub-matrices each of which refers to one
  # first-level folder
  subsets <- split(folder_data, folder_data[, 1])

  # Define helper function that drops the first column of a data frame and
  # converts the data frame to a vector of paths
  to_child_paths <- function(x) data_frame_to_paths(x[, -1, drop = FALSE])

  # Get a list of path vectors with the first level folders removed
  path_vectors <- lapply(subsets, function(x) remove_empty(to_child_paths(x)))

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
  # Paste columns and remove all trailing slashes
  gsub("/+$", "", kwb.utils::pasteColumns(df, sep = "/"))
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

  kwb.utils::writeText(paths, file.path(output_dir, file))
}
