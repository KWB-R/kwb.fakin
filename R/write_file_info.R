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
