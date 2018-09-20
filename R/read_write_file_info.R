# read_path_information --------------------------------------------------------
read_path_information <- function(file_info_dir)
{
  files <- dir(file_info_dir, "^path-info", full.names = TRUE)

  names(files) <- kwb.fakin:::extract_root_name(files)

  lapply(files, kwb.fakin::read_file_info)
}

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
#'
#' @export
#'
get_and_save_file_info <- function(root_dir, output_dir, check_dirs = TRUE)
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
  datetime_string <- format(Sys.time(), "%Y-%m-%d_%H%M")

  parent_folder <- basename(root_dir)

  filename <- sprintf("path-info_%s_%s.csv", datetime_string, parent_folder)

  file <- file.path(output_dir, filename)

  # Write the file information to a CSV file
  write_file_info(file_info, file)
}

# write_file_info --------------------------------------------------------------

#' Write File Information to CSV File
#'
#' @param file_info data frame as returned by
#'   \code{\link{get_recursive_file_info}}
#' @param file path to CSV file to be written
#' @param version determines which function to use for writing the CSV file
#'   1: \code{\link[utils]{write.table}}, 2: \code{\link[data.table]{fwrite}}
#'
#' @export
#'

write_file_info <- function(file_info, file, version = 2)
{
  kwb.utils::catAndRun(paste("Writing", file), {
    time_info <- system.time(

      file_info <- if (version == 1) {

        write_file_info_v1(file_info, file)

      } else if (version == 2) {

        write_file_info_v2(file_info, file)

      } else if (version == 3) {

        stop("Not implemented: version = ", version)
      }
    )
  })

  cat_elapsed(time_info)
}

# write_file_info_v1 -----------------------------------------------------------
write_file_info_v1 <- function(file_info, file)
{
  utils::write.table(
    file_info, file, row.names = FALSE, col.names = TRUE, sep = ";",
    na = ""
  )
}

# write_file_info_v2 -----------------------------------------------------------
write_file_info_v2 <- function(file_info, file)
{
  data.table::fwrite(file_info, file, sep = ";")
}

# read_file_info ---------------------------------------------------------------

#' Read File Information from CSV File
#'
#' @param file path to CSV file containing file information as stored with
#'   \code{\link{write_file_info}}
#' @param version determines which function to use for reading the CSV file
#'   1: \code{\link[utils]{read.table}}, 2: \code{\link[data.table]{fread}}
#'
#' @export
#'
read_file_info <- function(file, version = 2)
{
  kwb.utils::catAndRun(paste("Reading file information from", file), {

    time_info <- system.time(

      file_info <- if (version == 1) {

        read_file_info_v1(file)

      } else if (version == 2) {

        read_file_info_v2(file)

      } else {

        stop("Not implemented: version = ", version)
      }
    )
  })

  cat_elapsed(time_info)

  file_info
}

# read_file_info_v1 ------------------------------------------------------------
read_file_info_v1 <- function(file)
{
  utils::read.table(file, sep = ";", header = TRUE, stringsAsFactors = FALSE)
}

# read_file_info_v2 ------------------------------------------------------------
read_file_info_v2 <- function(file)
{
  as.data.frame(data.table::fread(file = file, sep = ";"))
}
