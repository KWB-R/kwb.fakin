# guess_file_metadata ----------------------------------------------------------

#' Guess Metadata about a Text File
#'
#' @param file path to text file
#' @param n_first_rows number of first rows of \code{file} from which to guess
#'   the meta information.
#' @return data frame with columns
#' \itemize{
#'   \item \code{paths}: does the file seem to contain path information, i.e.
#'   were slashes or backslashes found?
#'   \item \code{forbidden}: does the file contain characters that are forbidden
#'   in file paths?
#'   \item \code{header}: does the file seem to contain a header row?
#'   \item \code{windows}: are the paths given in "windows"-style, i.e. are the
#'     path segments separated by backslash?
#'   \item \code{sep}: column separator guessed
#'   \item \code{ncol}: number of columns guessed
#' } and attributes
#' \itemize{
#'   \item \code{file}: a copy of the file path given in \code{file}
#'   \item \code{first_rows}: first \code{n_first_rows} rows of \code{file}
#'   \item \code{columns} (optional): column headers if the file is assumed to
#'   contain a header row
#' }
#' @export
#'
guess_file_metadata <- function(file, n_first_rows = 1000)
{
  first_rows <- readLines(file, n_first_rows)
  patterns <- c(slash = "/", backslash = "\\\\")
  has_pattern <- lapply(patterns, grepl, first_rows)
  has_paths <- has_pattern$slash | has_pattern$backslash
  separators <- c(",", ";", "\t")

  sep_counts <- sapply(separators, function(sep) sum(stringi::stri_count(
    first_rows, regex = sep
  )))

  sep <- if (all(sep_counts == 0)) "" else separators[which.max(sep_counts)]

  ncol <- 1L

  if (nzchar(sep)) {
    ncol_frequency <- table(lengths(strsplit(first_rows, sep)))
    ncol <- as.integer(names(which.max(ncol_frequency)))
  }

  has_header <- if (any(has_paths)) ! has_paths[1] else FALSE

  metadata <- kwb.utils::noFactorDataFrame(
    paths = any(has_paths),
    forbidden = any(grepl("[<>?*]", first_rows)),
    header = has_header,
    windows = any(has_pattern$backslash),
    sep = sep,
    ncol = ncol
  )

  columns <- if (has_header) {
    strsplit(first_rows[1], sep)[[1]]
  }

  structure(metadata, file = file, first_rows = first_rows, columns = columns)
}
