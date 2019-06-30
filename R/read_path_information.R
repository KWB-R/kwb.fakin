# read_path_information --------------------------------------------------------

#' Read Files Containing File Path Information from Folder
#'
#' @param file_info_dir path to folder in which to look for files matching
#'   \code{pattern}
#' @param pattern pattern to match against the file names to be read. Default:
#'   "^path-info"
#' @param sep column separator passed to \code{\link{read_file_info}}
#' @param \dots further arguments passed to \code{\link{read_file_info}}
#' @importFrom kwb.file dir_full
#' @importFrom fs dir_ls
#' @export
#' @examples
#' # Set root directory (here: package installation directory of kwb.fakin)
#' root_dir <- system.file(package = "kwb.fakin")
#'
#' # Set output directory
#' output_dir <- tempdir()
#'
#' # Write all paths below root_dir into a "path-info"-file
#' kwb.fakin::get_and_save_file_info(root_dir, output_dir)
#'
#' # Read the "path-info"-files that are (now) found in output_dir
#' path_info <- kwb.fakin:::read_path_information(output_dir, sep = ";")
#'
read_path_information <- function(
  file_info_dir, pattern = "^path-info", sep = ",", ...
)
{
  files <- kwb.file::dir_full(file_info_dir, pattern)

  if (length(files) == 0) {

    message(sprintf(
      "No files matching '%s' in\n  '%s'\nAvailable files:\n  %s",
      pattern, file_info_dir, kwb.utils::stringList(
        basename(fs::dir_ls(file_info_dir, type = "file")), collapse = "\n  "
      )
    ))

    return (list())
  }

  names <- kwb.utils::removeExtension(basename(files))
  names <- gsub("path-info_\\d{4}-\\d{2}-\\d{2}_\\d{4}_", "", names)

  names(files) <- names

  lapply(files, read_file_info, sep = sep, ...)
}
