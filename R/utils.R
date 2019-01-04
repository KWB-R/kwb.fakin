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

# extdata_file -----------------------------------------------------------------
extdata_file <- function(file)
{
  system.file("extdata", file, package = "kwb.fakin")
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

# use_function_instead ---------------------------------------------------------
use_function_instead <- function(function_new, function_old)
{
  warning(call. = FALSE, sprintf(
    "Please use %s() instead of %s()",
    deparse(substitute(function_new)), deparse(substitute(function_old))
  ))
}

# vector_to_count_table --------------------------------------------------------
vector_to_count_table <- function(x)
{
  if (length(x) == 0) {

    return(NULL)
  }

  frequency <- table(x)

  frequency_data <- kwb.utils::asNoFactorDataFrame(frequency)

  unexpected <- ncol(frequency_data) != 2

  kwb.utils::printIf(unexpected, x)
  kwb.utils::printIf(unexpected, frequency)
  kwb.utils::printIf(unexpected, frequency_data)

  stats::setNames(frequency_data, c("name", "count"))
}
