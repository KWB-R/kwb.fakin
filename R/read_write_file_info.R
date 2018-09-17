# write_file_info --------------------------------------------------------------

# Write the file information to a CSV file
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

# Read the file information back into R
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
