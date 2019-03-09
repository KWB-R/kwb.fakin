#
# Very General Functions, Candidates for kwb.utils
#

# cat_elapsed ------------------------------------------------------------------
cat_elapsed <- function(time_info)
{
  cat("Elapsed:", time_info["elapsed"], "\n")
}

# cat_changes_if ---------------------------------------------------------------
cat_changes_if <- function(dbg, x, y)
{
  if (dbg) {

    cat_changes(x, y)
  }
}

# cat_changes ------------------------------------------------------------------
cat_changes <- function(x, y)
{
  is_modified <- x != y

  kwb.utils::catLines(sprintf("%s -> %s", x[is_modified], y[is_modified]))
}

# cat_time ---------------------------------------------------------------------
cat_time <- function(tag)
{
  cat(paste0("\n", tag, ":"), as.character(Sys.time()), "\n\n")
}

# check_or_set_ending_slash ----------------------------------------------------
check_or_set_ending_slash <- function(x)
{
  # Add slash to the end and replace multiple occurrences of slash at the end
  gsub("/+$", "/", paste0(x, "/"))
}

# cut_left ---------------------------------------------------------------------
cut_left <- function(x, start_string)
{
  string_length <- nchar(start_string)

  matching <- substr(x, 1, string_length) == start_string

  if (! all(matching)) {

    stop_(sprintf(
      "%d strings in '%s' do not start with '%s'!",
      sum(! matching), deparse(substitute(x)), start_string
    ))
  }

  substr(x, string_length + 1, nchar(x))
}

# extdata_file -----------------------------------------------------------------
extdata_file <- function(file)
{
  system.file("extdata", file, package = "kwb.fakin")
}

# indentation ------------------------------------------------------------------
indentation <- function(depth, space = "\t")
{
  paste(rep(space, depth), collapse = "")
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

# ncharTable -------------------------------------------------------------------
ncharTable <- function(x)
{
  table(nchar(x))
}

# ncharHist --------------------------------------------------------------------
ncharHist <- function(x)
{
  graphics::hist(nchar(x))
}

# read_csv ---------------------------------------------------------------------

#' Read Data from CSV File
#'
#' @param file path to CSV file
#' @param sep column separator
#' @param version determines which function to use for reading the CSV file
#'   1: \code{\link[utils]{read.table}}, 2: \code{\link[data.table]{fread}}
#' @param \dots further arguments passed to \code{\link[utils]{read.table}} or
#'   \code{\link[data.table]{fread}}
#'
#' @export
#'
read_csv <- function(file, sep = ";", version = 2, ...)
{
  message_string <- function(fun) sprintf("Reading '%s' with %s", file, fun)

  if (version == 1) {

    kwb.utils::catAndRun(
      message_string("utils::read.table()"),
      utils::read.table(
        file, header = TRUE, sep = sep, stringsAsFactors = FALSE, ...
      )
    )

  } else if (version == 2) {

    kwb.utils::catAndRun(
      message_string("data.table::fread()"),
      as.data.frame(data.table::fread(file = file, sep = sep, ...))
    )

  } else {

    stop_(
      "Invalid version (", version, "). Possible values are:\n",
      "  1 - use read.table() or\n",
      "  2 - use data.table::fread().\n"
    )
  }
}

# read_csv_de ------------------------------------------------------------------
read_csv_de <- function(file, ...)
{
  utils::read.csv2(file, stringsAsFactors = FALSE, ...)
}

# remove_duplicates ------------------------------------------------------------
remove_duplicates <- function(x)
{
  is_duplicate <- duplicated(x)

  if (! any(is_duplicate)) {
    return(x)
  }

  kwb.utils::catAndRun(
    messageText = paste("Removing", sum(is_duplicate), "duplicate(s)"),
    expr = x[! is_duplicate]
  )
}

# remove_empty -----------------------------------------------------------------
remove_empty <- function(x, dbg = FALSE)
{
  is_empty <- (x == "")

  if (! any(is_empty)) {

    return(x)
  }

  kwb.utils::catIf(dbg, sum(is_empty), "elements removed.\n")

  x[! is_empty]
}

# sort_unique ------------------------------------------------------------------
sort_unique <- function(x)
{
  sort(unique(x))
}

# split_string_into_parts ------------------------------------------------------
split_string_into_parts <- function(x, size = 80)
{
  n <- nchar(x)

  if (n > size) {

    starts <- size * (seq_len((n - 1) %/% size + 1) - 1)

    bounds <- kwb.utils::startsToRanges(starts, n, 1, 0)

    apply(bounds, MARGIN = 1, function(ii) substr(x, ii[1], ii[2]))

  } else {

    x
  }
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

# write_csv --------------------------------------------------------------------

#' Write Data Frame to CSV File
#'
#' @param data data frame
#' @param file path to CSV file to be written
#' @param sep column separator
#' @param version determines which function to use for writing the CSV file
#'   1: \code{\link[utils]{write.table}}, 2: \code{\link[data.table]{fwrite}}
#' @param \dots further arguments passed to \code{\link[utils]{write.table}} or
#'   \code{\link[data.table]{fwrite}}
#' @export
#'
write_csv <- function(data, file, sep = ";", version = 2, ...)
{
  message_string <- function(fun) sprintf("Writing to '%s' with %s", file, fun)

  if (version == 1) {

    kwb.utils::catAndRun(
      message_string("utils::write.table()"),
      utils::write.table(
        data, file, row.names = FALSE, col.names = TRUE, sep = sep, na = "",
        ...
      )
    )

  } else if (version == 2) {

    kwb.utils::catAndRun(
      message_string("data.table::fwrite()"),
      data.table::fwrite(data, file, sep = sep, ...)
    )

  } else {

    stop_(
      "Invalid version (", version, "). Possible values are:\n",
      "  1 - use write.table() or\n",
      "  2 - use data.table::fwrite().\n"
    )
  }
}
