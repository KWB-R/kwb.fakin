#' Read CSV File Created With Powershell From Search Index
#'
#' @param file file containing the output of a Windows powershell script that
#'   reads file paths from the search index database and writes them to a CSV
#'   file
#'
#' @return data frame
#'
read_file_info_search_index <- function(file)
{
  file_info <- read_csv(file, sep = ",", version = 2)

  names(file_info) <- gsub("^SYSTEM[.]", "", names(file_info))

  file_info$ITEMURL <- gsub("^file:", "", file_info$ITEMURL)

  file_info
}

#' Read CSV File Created Directly With libuv
#'
#' @param file file containing the output of my own fast version of
#'   \code{fs::dir_info}
#'
#' @return data frame with time columns converted to \code{POSIXct}
#'
read_file_info_libuv <- function(file)
{
  #file <- "~/CProgramming/fs/files_hauke.csv"

  file_info <- utils::read.csv(file, stringsAsFactors = FALSE)

  time_columns <- grep("tim$", names(file_info), value = TRUE)

  file_info[time_columns] <- lapply(
    file_info[time_columns], as.POSIXct, origin = "1970-01-01"
  )

  file_info
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
  time_info <- system.time(
    write_csv(file_info, file, sep = ";", version = version)
  )

  cat_elapsed(time_info)
}
