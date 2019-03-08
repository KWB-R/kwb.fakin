# prepare_path_data ------------------------------------------------------------

#' Select and Filter for Relevant Path Information
#'
#' This function gets a data frame containing path information as input. It
#' filters for rows with value "file" in column \code{type} and keeps only the
#' columns \code{path} and \code{size}. If \code{pattern} is not \code{NULL},
#' the data frame is then filtered for rows in which \code{path} matches the
#' given pattern. Finally, the common root of all paths in column \code{path} is
#' removed and the resulting data frame is returned.
#'
#' @param path_info data frame containing file path information as returned by
#'   \code{\link{read_file_info}}
#' @param pattern pattern by which to select a subset of paths or \code{NULL}
#'   (default) if all paths in \code{path_info} are to be considered. By
#'   setting the pattern to "^/path/to/start/directory" you can "zoom into" the
#'   path tree, returning only the contents of "/path/to/start/directory".
#'
#' @return data frame with columns \code{path} and \code{size}. See Description.
#' @examples
#' path_info <- kwb.utils::noFactorDataFrame(
#'   path = c("/path/to/root/", "/path/to/root/file_1", "/path/to/root/file_2"),
#'   type = c("directory", "file", "file"),
#'   size = c(0L, 10L, 20L)
#' )
#'
#' path_info
#'
#' kwb.fakin:::prepare_path_data(path_info)
prepare_path_data <- function(path_info, pattern = NULL)
{
  # Get a vector of path types (directory or file)
  types <- kwb.utils::selectColumns(path_info, "type")

  # Keep only paths to files
  path_data <- path_info[types == "file", ]

  # Select only required columns
  path_data <- kwb.utils::selectColumns(path_data, c("path", "size"))

  # Filter for paths matching the pattern if a pattern is given
  if (! is.null(pattern)) {

    path_data <- path_data[grepl(pattern, path_data$path), ]
  }

  # Cut off the first segments of the common root path
  path_data$path <- kwb.file::remove_common_root(path_data$path, n_keep = 1)

  path_data
}
