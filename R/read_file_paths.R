# read_file_paths --------------------------------------------------------------

#' Read File Paths from a File
#'
#' The function tries to guess what type of file is given to the function and
#' calls the appropriate function to read the file. The aim of this function is
#' to provide a common result format independent from the type of file that was
#' read.
#'
#' @param file file containing file path information (path only or additional
#'   information such as file type, size or creation/modification time, etc.)
#' @param metadata data frame containing metadata about the file. If given, it
#'   must look as what \code{\link{guess_file_metadata}} returns. If \code{NULL}
#'   the same function is called to guess metadata about the file.
#' @return data frame with columns...
#' @export
#'
read_file_paths <- function(file, metadata = NULL)
{
  if (is.null(metadata)) {
    metadata <- guess_file_metadata(file)
  }

  if (! is_valid_path_file(metadata)) {
    return (NULL)
  }

  metadata$encoding <- kwb.utils::defaultIfNULL(
    metadata$encoding, guess_expected_encoding(
      file, expected = c("ISO-8859-1", "WINDOWS-1252")
    )
  )

  if (metadata$ncol == 1) {

    paths <- read_paths_(file, fileEncoding = metadata$encoding)
    result <- kwb.utils::noFactorDataFrame(path = paths)
    result$type <- guess_file_path_type(x = result$path)
    result$size <- ifelse(result$type == "directory", 0L, 2^20)

  } else {

    columns <- attr(metadata, "columns")

    result <- if ("birthtim" %in% columns) {

      read_file_info_libuv(file)

    } else if ("ITEMURL" %in% columns) {

      read_file_info_search_index(file)

    } else {

      read_file_info_(
        file, sep = metadata$sep, fileEncoding = metadata$encoding_fread
      )
    }
  }

  result
}

# is_valid_path_file -----------------------------------------------------------
is_valid_path_file <- function(metadata)
{
  get <- kwb.utils::selectElements

  is_valid <- get(metadata, "paths") && ! get(metadata, "forbidden")

  if (! is_valid) {

    format_string <- paste0(
      "The file does not seem to contain valid paths. Returning NULL.\n",
      "File: %s\n",
      "Folder: %s\n",
      "First %d rows:\n%s\n"
    )

    file <- kwb.utils::getAttribute(metadata, "file")
    first_rows <- kwb.utils::getAttribute(metadata, "first_rows")

    message(sprintf(
      format_string, basename(file), dirname(file), length(first_rows),
      paste(collapse = "\n", first_rows)
    ))
  }

  is_valid
}

# guess_file_path_type ---------------------------------------------------------
guess_file_path_type <- function(x)
{
  kwb.utils::catAndRun("Guessing file path type", {
    has_final_slash <- grepl_bytes("/$", x)
    has_extension <- kwb.utils::fileExtension(x) != ""
    ifelse(has_final_slash | ! has_extension, "directory", "file")
  })
}

