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
