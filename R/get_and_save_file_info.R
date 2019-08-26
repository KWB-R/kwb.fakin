# get_and_save_file_info -------------------------------------------------------

#' Get and Save File Information
#'
#' @param root_dir path to the directory from which to start searching for
#'   files
#' @param output_dir path to the output directory. In this directory, a file
#'   "path-info_<date-time>_<parent-folder>.csv" will be generated with
#'   <date-time> being a date and time string in yyyy-mm-dd_HHMM format and
#'   <parent-folder> being the last path segment of \code{root_dir}
#' @param check_dirs if \code{TRUE} (default) it is checked in advance if both
#'   \code{root_dir} and \code{output_dir} exist. Switch this off if e.g.
#'   network paths are wrongly considered to be non-existing.
#' @param format format string specifying how to format the part of the
#'   filename intended to contain date (and, if required, time) information.
#'   Default: "\%Y-\%m-\%d_\%H\%M"
#' @export
#'
get_and_save_file_info <- function(
  root_dir, output_dir, check_dirs = TRUE, format = "%Y-%m-%d_%H%M"
)
{
  # Check if the root directory exists
  if (check_dirs) {

    kwb.utils::safePath(root_dir)
    kwb.utils::safePath(output_dir)
  }

  # Get information on all files in this directory
  runtime <- system.time(file_info <- get_recursive_file_info(root_dir))

  cat_elapsed(runtime)

  # Define path to output file
  datetime_string <- format(Sys.time(), format = format)

  parent_folder <- basename(root_dir)

  filename <- sprintf("path-info_%s_%s.csv", datetime_string, parent_folder)

  file <- file.path(output_dir, filename)

  # Write the file information to a CSV file
  write_file_info(file_info, file)
}
