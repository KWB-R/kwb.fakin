# read_lines -------------------------------------------------------------------

#' Read Lines by Giving the File Encoding
#'
#' @param file a connection object or character string
#' @param \dots arguments passed to \code{\link{readLines}}
#' @param encoding passed to \code{\link{readLines}}.
#' @param fileEncoding The name of the encoding to be assumed. Passed as
#'   \code{encoding} to \code{\link{file}}, see there.
#' @export
#'
read_lines <- function(file, ..., encoding = "unknown", fileEncoding = "")
{
  # This part is copied from the implementation of read.table
  if (is.character(file)) {
    con <- if (nzchar(fileEncoding)) {
      file(file, "rt", encoding = fileEncoding)
    } else {
      file(file, "rt")
    }
    on.exit(close(con))
  } else {
    con <- file
  }

  readLines(con, encoding = encoding, ...)
}

# default_local_encoding -------------------------------------------------------
default_local_encoding <- function(dbg = TRUE)
{
  encodings <- utils::localeToCharset()

  kwb.utils::catIf(dbg && length(encodings) > 1, sprintf(
    "Suggested encodings: %s\n", kwb.utils::stringList(encodings)
  ))

  encoding <- kwb.utils::defaultIfNA(encodings[1], "unknown")

  kwb.utils::catIf(dbg, sprintf("Selected encoding: '%s'\n", encoding))

  encoding
}
