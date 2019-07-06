# read_paths -------------------------------------------------------------------

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
#'
#' @importFrom kwb.utils catAndRun
#' @importFrom kwb.utils catIf
#' @importFrom kwb.utils stringList
#'
#' @export
#'
read_paths <- function(
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

# writePathsToFiles ------------------------------------------------------------
writePathsToFiles <- function(xall, file)
{
  for (i in seq_along(xall)) {

    file <- sub(".txt", paste0("_", LETTERS[i], ".txt"), file)

    kwb.utils::writeText(xall[[i]], file = file)
  }
}
