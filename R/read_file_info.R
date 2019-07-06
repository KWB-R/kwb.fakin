# read_file_info ---------------------------------------------------------------

#' Deprecated
#'
#' @param \dots arguments passed to private function
#' @export
#' @keywords internal
#'
read_file_info <- function(...)
{
  kwb.utils::warningDeprecated("read_file_info", "read_file_paths")
  read_file_info_(...)
}

# read_file_info_ --------------------------------------------------------------

#' Read File Information from CSV File
#'
#' @param file path to CSV file containing file information as stored with
#'   \code{\link{write_file_info}}
#' @param version passed to \code{\link{read_csv}}
#' @param \dots further arguments passed to \code{\link{read_csv}})
#' @importFrom kwb.utils catAndRun
#' @keywords internal
#'
read_file_info_ <- function(file, version = 2, ...)
{
  file_info <- read_csv(file, version = version, ...)

  # If the file has been created with PowerShell, adapt the format
  if ("FullName" %in% names(file_info)) {

    file_info <- kwb.utils::catAndRun("Reformatting the file info table", {
      reformat_file_info(file_info)
    })
  }

  # Convert size in bytes to size in MB (to avoid integer64)
  file_info$size <- kwb.utils::catAndRun("Converting file size to MiB", {
    kwb.utils::selectColumns(file_info, "size") / 1024^2
  })

  structure(file_info, units = list(size = "MiB (2^20 Bytes)"))
}

# reformat_file_info -----------------------------------------------------------

#' @importFrom kwb.utils defaultIfNA renameColumns rStylePath selectColumns
#' @importFrom lubridate dmy_hms
#' @keywords internal
#'
reformat_file_info <- function(file_info)
{
  # Which columns are time columns?
  is_time <- grepl("Utc$", names(file_info))

  # Convert character timestamps to POSIXct
  file_info[is_time] <- lapply(file_info[is_time], lubridate::dmy_hms)

  # Set column "type" to either "file" or "directory"
  attribs <- kwb.utils::selectColumns(file_info, "Attributes")
  file_info$type <- ifelse(grepl("Directory", attribs), "directory", "file")

  # Rename columns
  file_info <- kwb.utils::renameColumns(file_info, list(
    Length = "size", FullName = "path"
  ))

  # Set the path delimiters to forward slashes
  paths <- kwb.utils::selectColumns(file_info, "path")
  file_info$path <- kwb.utils::rStylePath(paths)

  # Set NA to 0 in column size
  file_info$size <- kwb.utils::defaultIfNA(file_info$size, 0)

  # Move main columns to the front
  kwb.utils::moveColumnsToFront(file_info, c("path", "type", "size"))
}

#' Read CSV File Created With Powershell From Search Index
#'
#' @param file file containing the output of a Windows powershell script that
#'   reads file paths from the search index database and writes them to a CSV
#'   file
#' @return data frame
#' @keywords internal
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
#' @return data frame with time columns converted to \code{POSIXct}
#' @keywords internal
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
