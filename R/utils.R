#
# Very General Functions, Candidates for kwb.utils
#

# cat_elapsed ------------------------------------------------------------------
cat_elapsed <- function(time_info)
{
  cat("Elapsed:", time_info["elapsed"], "\n")
}

# catTime ----------------------------------------------------------------------
catTime <- function(tag)
{
  cat(paste0("\n", tag, ":"), as.character(Sys.time()), "\n\n")
}

# getElementLengths ------------------------------------------------------------

#' Get the Lenghts of List Elements
#'
#' @param x a list
#' @return vector of integer
#'
getElementLengths <- function(x)
{
  if (! is.list(x)) {

    stop_(sprintf(
      "The object '%s' given to getElementLengths() is not a list but: '%s'",
      deparse(substitute(x)), mode(x)
    ))
  }

  if (length(x) == 0) {

    integer()

  } else {

    sapply(x, length)
  }
}

# left_substring_equals --------------------------------------------------------

#' Is Left Substring of X Equal To Y?
#'
#' @param x String of which the left part is compared with \code{y}
#' @param y String to be compared with the left part of \code{x}
#'
left_substring_equals <- function(x, y)
{
  stopifnot(is.character(x), is.character(y))

  substr(x, 1, nchar(y)) == y
}

# stop_ ------------------------------------------------------------------------
stop_ <- function(...)
{
  stop(..., call. = FALSE)
}

# toDataFrame ------------------------------------------------------------------
toDataFrame <- function(x)
{
  if (is.list(x)) {

    do.call(data.frame, c(x, stringsAsFactors = FALSE))

  } else {

    data.frame(x = x, stringsAsFactors = FALSE)
  }
}

# vector_to_count_table --------------------------------------------------------
vector_to_count_table <- function(x)
{
  frequency <- sort(table(x), decreasing = TRUE)

  stats::setNames(kwb.utils::asNoFactorDataFrame(frequency), c("name", "count"))
}
