#
# Very General Functions, Candidates for kwb.utils
#

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

  sapply(x, length)
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

# catTime ----------------------------------------------------------------------
catTime <- function(tag)
{
  cat(paste0("\n", tag, ":"), as.character(Sys.time()), "\n\n")
}
