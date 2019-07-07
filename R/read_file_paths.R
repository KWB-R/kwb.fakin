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
      file, expected = c("ISO-8859-1", "WINDOWS-1252", "ASCII")
    )
  )

  # Case: One path per line
  if (metadata$ncol == 1) return (kwb.utils::catAndRun(
    "Reading file with read_paths_only()",
    read_paths_only(file, metadata)
  ))

  # If we arrive here, there is more than one column
  columns <- attr(metadata, "columns")

  # Case: File created with Linux-tool libuv
  if ("birthtim" %in% columns) return (kwb.utils::catAndRun(
    "Reading file with read_file_info_libuv()",
    read_file_info_libuv(file)
  ))

  # Case: File created by querying the Windows search index
  if ("ITEMURL" %in% columns || any(grepl("SYSTEM[.]", columns))) return (
    kwb.utils::catAndRun("Reading file with read_file_info_search_index()", {
      result <- read_file_info_search_index(file)
      result <- renameColumns(result, list(
        ITEMTYPE = "type", ITEMPATHDISPLAY = "path", SIZE = "size"
      ))
      result$type <- ifelse(result$type == "Directory", "directory", "file")
      result
    })
  )

  # Case: File created by Windows Powershell script, running as a cron-job
  return (kwb.utils::catAndRun(
    "Reading file with read_file_info()", read_file_info_(
      file, sep = metadata$sep, fileEncoding = metadata$encoding_fread
    )
  ))

  #normalise_file_info_data(result)
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
