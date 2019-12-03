#
# Very General Functions, Candidates for kwb.utils
#

# add_percentage_of_sum_columns ------------------------------------------------
add_percentage_of_sum_columns <- function(df, columns)
{
  for (column in columns) {
    df[[paste0("perc_", column)]] <- kwb.utils::percentageOfSum(
      kwb.utils::selectColumns(df, column)
    )
  }

  df
}

# bytes_to_mib -----------------------------------------------------------------
bytes_to_mib <- function(x)
{
  x / 2^20
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

#' Path to File in extdata Folder of this Package
#'
#' @param file filename or path to the file, relative to extdata/
#' @export
#'
extdata_file <- function(file)
{
  paths <- system.file("extdata", file, package = "kwb.fakin")

  if (all(paths == "") || length(paths) != length(file)) stop_(
    "Not all files could be found in the extdata folder. Available files:\n  ",
    kwb.utils::stringList(collapse = "\n  ", dir(
      system.file("extdata", package = "kwb.fakin")
    ))
  )

  paths
}

# extend_each_element ----------------------------------------------------------
extend_each_element <- function(x, ...)
{
  stopifnot(is.list(x))
  stopifnot(all(sapply(x, is.list)))

  lapply(x, function(xx) {
    kwb.utils::hsRestoreAttributes(c(xx, ...), attributes(xx))
  })
}

# fails ------------------------------------------------------------------------
fails <- function(expr)
{
  inherits(try(expr), "try-error")
}

# indentation ------------------------------------------------------------------
indentation <- function(depth, space = "\t")
{
  paste(rep(space, depth), collapse = "")
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
#' @param fileEncoding passed to \code{\link[utils]{read.table}} or as
#'   \code{encoding} to \code{\link[data.table]{fread}}
#' @param \dots further arguments passed to \code{\link[utils]{read.table}} or
#'   \code{\link[data.table]{fread}}
#'
#' @export
#'
read_csv <- function(file, sep = ";", version = 2, fileEncoding = NULL, ...)
{
  message_string <- function(fun) sprintf("Reading '%s' with %s", file, fun)

  fileEncoding <- kwb.utils::defaultIfNULL(
    fileEncoding, ifelse(version == 1, "", "unknown")
  )

  if (version == 1) {

    kwb.utils::catAndRun(
      message_string("utils::read.table()"),
      utils::read.table(
        file, header = TRUE, sep = sep, stringsAsFactors = FALSE,
        fileEncoding = fileEncoding, ...
      )
    )

  } else if (version == 2) {

    kwb.utils::catAndRun(
      message_string("data.table::fread()"),
      as.data.frame(data.table::fread(
        file = file, sep = sep, encoding = fileEncoding, ...
      ))
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

# succeeds ---------------------------------------------------------------------
succeeds <- function(expr)
{
  ! fails(expr)
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
