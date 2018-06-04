# startsWithParts --------------------------------------------------------------
#' Does a strsplit list start with given sequence of elements?
#'
#' @param parts list of list of character as returned by \code{strsplit}
#' @param elements vector of character giving the sequence of strings to be
#'   found in \code{parts}
#' @return vector of logical as long as \code{parts} containing \code{TRUE} at
#'   positions \code{i} for which \code{all(parts[[i]][seq_along(elements)] ==
#'   elements)} is \code{TRUE}
#' @export
#' @examples
#' parts <- strsplit(c("a/b/c", "a/b/d", "b/c"), "/")
#' startsWithParts(parts, c("a", "b"))
#' startsWithParts(parts, c("b", "c"))
startsWithParts <- function(parts, elements)
{
  stopifnot(is.list(parts) || is.matrix(parts))
  stopifnot(all(! is.na(elements)))

  indices <- seq_along(elements)

  if (is.list(parts)) {
    selected <- rep(TRUE, length(parts))
    for (i in indices) {
      selected <- selected & sapply(parts, "[", i) == elements[i]
    }
  } else {
    selected <- rep(TRUE, nrow(parts))
    for (i in seq_along(elements)) {
      selected <- selected & ! is.na(parts[, i]) & (parts[, i] == elements[i])
    }
  }

  selected
}

# isDotOrDoubleDot -------------------------------------------------------------
#' Does a string end with one or two dots (".", "..")?
#'
#' @param x vector of character
#' @return vector of logical
#' @export
#' @examples
#'
#' isDotOrDoubleDot(c("a", "b.", "c..", "d", "efg..h"))
isDotOrDoubleDot <- function(x)
{
  grepl("\\.\\.?$", x)
}

# isASCII ----------------------------------------------------------------------
#' Do Strings Consist only of ASCII Characters?
#'
#' The code has been "stolen" from \code{\link[tools]{showNonASCII}}
#'
#' @param x vector of character
#' @return vector of logical with \code{TRUE} at positions \code{i} where
#' \code{x[i]} contains only ASCII characters
#' @export
#' @examples
#' months <- c("Januar", "Februar", "M\xe4rz")
#'
#' cat(months, "\n")
#'
#' isASCII(months)
isASCII <- function(x)
{
  asc <- iconv(x, "latin1", "ASCII")
  ! is.na(asc) & asc == x
}

# isPlaceholder ----------------------------------------------------------------
isPlaceholder <- function(x)
{
  grepl("^<[^<>]+>$", x)
}
