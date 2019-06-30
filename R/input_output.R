# read_paths -------------------------------------------------------------------

#' Read Paths From File
#'
#' Read paths from a file that contains one path per line. Convert backslashes
#' to slashes and sort the paths (by default)
#'
#' @param file full path to the text file containing paths
#' @param encoding passed to \code{\link[base]{readLines}}
#' @param fileEncoding passed to \code{\link{file}}
#' @param do_sort if \code{TRUE} (default), the vector of paths is sorted
#'   alphanumerically.
#'
#' @return vector of character
#'
#' @importFrom kwb.utils catAndRun
#' @importFrom kwb.utils catIf
#' @importFrom kwb.utils stringList
#'
#' @export
#'
read_paths <- function(file, encoding = NULL, fileEncoding = "", do_sort = TRUE)
{
  if (is.null(encoding)) {

    encodings <- utils::localeToCharset()

    kwb.utils::catIf(length(encodings) > 1, sprintf(
      "Suggested encodings: %s\n", kwb.utils::stringList(encodings)
    ))

    encoding <- kwb.utils::defaultIfNA(encodings[1], "unknown")
  }

  cat(sprintf("Selected encoding: '%s'\n", encoding))

  # This part is copied from the implementation of read.table
  if (is.character(file)) {
    con <- if (nzchar(fileEncoding))
      file(file, "rt", encoding = fileEncoding)
    else
      file(file, "rt")
    on.exit(close(con))
  } else {
    con <- file
  }

  paths <- kwb.utils::catAndRun(
    messageText = sprintf("Reading paths from '%s'", file),
    expr = readLines(con, encoding = encoding)
  )

  cat(length(paths), "lines have been read.\n")

  has_backslash <- grepl("[\\]", paths)

  if (any(has_backslash)) {
    kwb.utils::catAndRun("Converting backslashes to slashes", {
      paths[has_backslash] <- gsub("[\\]", "/", paths[has_backslash])
    })
  }

  if (do_sort) {
    catAndRun("Sorting paths", paths <- sort(paths))
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
