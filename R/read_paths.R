# read_paths -------------------------------------------------------------------

#' Deprecated
#'
#' @param \dots passed to private function
#' @export
#' @keywords internal
#'
read_paths <- function(...)
{
  kwb.utils::warningDeprecated("read_paths", "read_file_paths")
  read_paths_(...)
}

# read_paths_ ------------------------------------------------------------------

#' Read Paths From File
#'
#' Read paths from a file that contains one path per line. Convert backslashes
#' to slashes and sort the paths (by default)
#'
#' @param file full path to the text file containing paths
#' @param fileEncoding The name of the encoding to be assumed. Passed as
#'   \code{encoding} to \code{\link{file}}, see there.
#' @param encoding passed to \code{\link{readLines}}.
#' @param \dots arguments passed to \code{\link{read_lines}}, such as
#'   \code{n}
#' @param do_sort if \code{TRUE} (default), the vector of paths is sorted
#'   alphanumerically.
#' @param expected_encodings vector of names of file encodings that are
#'   expected to occur. Expected encodings will be preferred.
#' @return vector of character
#' @importFrom kwb.utils catAndRun
#' @importFrom kwb.utils catIf
#' @importFrom kwb.utils stringList
#'
read_paths_ <- function(
  file, fileEncoding = NULL, encoding = NULL, ..., do_sort = TRUE,
  expected_encodings = c("windows-1252", "ISO-8859-1")
)
{
  encoding <- kwb.utils::defaultIfNULL(encoding, default_local_encoding())

  if (is.null(fileEncoding)) {
    fileEncoding <- guess_expected_encoding(file, expected_encodings)
  }

  paths <- kwb.utils::catAndRun(sprintf("Reading paths from '%s'", file), {
    read_lines(file, encoding = encoding, fileEncoding = fileEncoding, ...)
  })

  cat(length(paths), "lines have been read.\n")

  has_backslash <- grepl("[\\]", paths)

  if (any(has_backslash)) {
    kwb.utils::catAndRun("Converting backslashes to slashes", {
      paths[has_backslash] <- gsub("[\\]", "/", paths[has_backslash])
    })
  }

  if (do_sort) {
    paths <- catAndRun("Sorting paths", sort(paths))
  }

  paths
}
