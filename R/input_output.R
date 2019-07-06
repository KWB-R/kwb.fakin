# read_paths -------------------------------------------------------------------

#' Read Paths From File
#'
#' Read paths from a file that contains one path per line. Convert backslashes
#' to slashes and sort the paths (by default)
#'
#' @param file full path to the text file containing paths
#' @param \dots arguments passed to \code{\link{read_lines}}, such as
#'   \code{n}, \code{fileEncoding}, \code{encoding}
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
read_paths <- function(file, ..., do_sort = TRUE)
{
  paths <- kwb.utils::catAndRun(sprintf("Reading paths from '%s'", file), {
    read_lines(file, ...)
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
