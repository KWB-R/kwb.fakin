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
  normalise_file_info(file_info)
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
  result <- read_csv(file, sep = ",", version = 2)

  names(result) <- gsub("^SYSTEM[.]", "", names(result))

  if (! is.null(result$ITEMURL)) {
    result$ITEMURL <- gsub("^file:", "", result$ITEMURL)
  }

  result
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
