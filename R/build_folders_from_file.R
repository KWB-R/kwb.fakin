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
#' writeLines(paths, file <- tempfile())
#'
#' # Create a temporary target directory
#' target_dir <- kwb.utils::createDirectory(file.path(tempdir(), "test"))
#'
#' # Create the folder structure as defined by the paths in the temporary file
#' kwb.fakin::build_folders_from_file(file, target_dir)
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
  #pattern = NULL; max_depth = NULL; encoding = "Latin-1"
  #kwb.utils::assignPackageObjects("kwb.fakin")
  paths_raw <- read_paths_(file, do_sort = FALSE, encoding = encoding)

  path_list <- pathlist::pathlist(if (is.null(pattern)) {
    paths_raw
  } else {
    grep(pattern, paths_raw, ignore.case = TRUE, value = TRUE)
  })

  if (is.null(max_depth)) {
    max_depth <- max(pathlist::depth(path_list))
  }

  kwb.utils::createDirectory(target_dir)

  write_paths_to_folder_tree(path_list, target_dir, max_depth)
}

# write_paths_to_folder_tree ---------------------------------------------------
write_paths_to_folder_tree <- function(
  path_list, target_dir, max_depth = 2L, depth = 0L
)
{
  stopifnot(inherits(path_list, "pathlist"))

  if (length(path_list) == 0) {
    return()
  }

  # Names of top levels
  top_levels <- unique(pathlist::as.matrix(path_list, relative = TRUE)[, 1])

  # Path subsetting method
  dollar <- methods::getMethod("$", "pathlist")

  # Loop through the top level folders
  for (top_level in top_levels) {

    #top_level <- top_levels[1]
    pl <- dollar(path_list, top_level)

    # If we are already at maximal depth, write one file per top level folder
    # containing the paths to all subfolders in each top level folder
    if (depth == max_depth) {

      relative_paths <- as.character(pl, relative = TRUE)

      n <- length(pl)

      file <- file.path(target_dir, sprintf(
        "%s__WITH%s-SUBFOLDER%s.txt",
        top_level, ifelse(n > 0, paste0("-", n), "OUT"), ifelse(n == 1, "", "S")
      ))

      kwb.utils::writeText(relative_paths, file = file)

    } else {

      # Create the top level folder and call this function recursively
      new_dir <- kwb.utils::createDirectory(file.path(target_dir, top_level))

      write_paths_to_folder_tree(
        path_list = pl, target_dir = new_dir, max_depth = max_depth,
        depth = depth + 1L
      )
    }
  }
}
