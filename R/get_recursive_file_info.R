# get_recursive_file_info ------------------------------------------------------

#' Call file.info recursively on files below a root folder
#'
#' @param root_dir path to the root directory from which to start the recursive
#'   search for files
#' @param pattern regular expression matching the names of the files to be
#'   considered. By default, all files are considered.
#' @param dbg if \code{TRUE} (default) progress messages are shown
#'
#' @export
#'
get_recursive_file_info <- function(root_dir, pattern = NULL, dbg = TRUE)
{
  roots <- list.dirs(root_dir)

  # For each directory separately, get the paths to the files first and the
  # properties of these files second
  file_info <- do.call(rbind, lapply(roots, function(root) {

    kwb.utils::catAndRun(paste("Browsing", root), dbg = dbg, file.info(
      list.files(root, full.names = TRUE, recursive = TRUE),
      extra_cols = TRUE
    ))
  }))

  stopifnot(all(! file_info$isdir))

  file_info
}

# add_path_column --------------------------------------------------------------
add_path_column <- function(file_info)
{
  paths <- removeCommonRoot(rownames(file_info), keep_root = TRUE)

  kwb.utils::resetRowNames(kwb.utils::setColumns(file_info, pathString = paths))
}
