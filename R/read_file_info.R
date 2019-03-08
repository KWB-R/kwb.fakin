# read_file_info ---------------------------------------------------------------

#' Read File Information from CSV File
#'
#' @param file path to CSV file containing file information as stored with
#'   \code{\link{write_file_info}}
#' @param version passed to \code{\link{read_csv}}
#' @param \dots further arguments passed to \code{\link{read_csv}})
#'
#' @export
#'
#' @importFrom kwb.utils catAndRun
#'
read_file_info <- function(file, version = 2, ...)
{
  time_info <- system.time(file_info <- read_csv(file, version = version, ...))

  cat_elapsed(time_info)

  # If the file has been created with PowerShell, adapt the format
  if ("FullName" %in% names(file_info)) {

    kwb.utils::catAndRun(
      "Reformatting the file info table",
      reformat_file_info(file_info)
    )

  } else {

    file_info
  }
}

# reformat_file_info -----------------------------------------------------------

#' @importFrom kwb.utils defaultIfNA renameColumns rStylePath selectColumns
#' @importFrom lubridate dmy_hms
#' @keywords internal
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
