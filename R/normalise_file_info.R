# normalise_file_info ----------------------------------------------------------
normalise_file_info <- function(file_info)
{
  # Provide the column names
  columns <- names(file_info)

  # If the file has been created with PowerShell, adapt the format
  if ("FullName" %in% columns) {

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
