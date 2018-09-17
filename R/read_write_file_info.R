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
  data.table::fread(file = file, sep = ";")
}
