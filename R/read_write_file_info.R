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
