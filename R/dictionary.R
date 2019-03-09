# getDictOneByOne --------------------------------------------------------------

#' Get a Path Dictionary
#'
#' @param paths vector of character strings representing file or folder paths
#' @param n number of compression levels
#'
getDictOneByOne <- function(paths, n = 10)
{
  # Get the frequencies of the directory paths
  frequencies <- getFrequencies(paths = paths, first.only = TRUE)

  frequency_data_raw <- toFrequencyData(frequencies)

  frequency_data <- frequency_data_raw

  dictionary <- list()

  while (length(dictionary) < n) {

    # Next key to be used in the dictionary
    key <- toKey(length(dictionary) + 1)

    # Rescore and reorder frequency_data
    frequency_data <- rescore_and_reorder_frequency_data(frequency_data, key)

    printFreqs(frequency_data)

    # Which is the "winning" path?
    winner <- frequency_data[1, ]

    # Remove the winning path
    frequency_data <- frequency_data[-1, ]

    # Put the winning path into the dictionary
    dictionary[[key]] <- winner$path

    winner <- cbind(i = length(dictionary), key = key, winner)

    winner <- kwb.utils::moveColumnsToFront(winner, main_columns_winner())

    print(winner, row.names = FALSE)

    # Update the length (reduce by "shortage", the number of saved characters)
    frequency_data <- update_frequency_data_length(frequency_data, winner, key)
  }

  dictionary
}

# getFrequencies ---------------------------------------------------------------
getFrequencies <- function(
  subdirs = kwb.file::split_paths(paths), paths = NULL, first.only = TRUE,
  dbg = TRUE
)
{
  n_levels <- kwb.utils::getElementLengths(subdirs)

  if (dbg) {

    main <- "Distribution of path depths"

    graphics::hist(n_levels, main = main)

    kwb.utils::printIf(dbg, table(n_levels), main)
  }

  lapply(seq_len(max(n_levels)), function(i) {

    is_long_enough <- n_levels >= i

    kwb.utils::catIf(dbg, sprintf("i = %d, n = %d...\n", i, sum(is_long_enough)))

    x <- sapply(subdirs[is_long_enough], function(xx) {

      kwb.utils::collapsed(xx[seq_len(i)], "/")
    })

    y <- sortedImportance(x)

    kwb.utils::printIf(dbg, utils::head(y))

    if (first.only) y[1] else y
  })
}

# sortedImportance -------------------------------------------------------------

#' Importance of Strings
#'
#' Decreasingly sorted frequencies of strings, by default weighted by their
#' length
#'
#' @param x vector of character strings
#' @param weighted if \code{TRUE} (default) the frequencies of strings are
#'  multiplied by the corresponding string lengths
#'
#' @return named integer vector (of class table) containing the decreasingly
#'   sorted importance values of the elements in \code{x}. The importance of a
#'   string is either its frequency in \code{x} (if weighted is FALSE) or the
#'   product of this frequency and the string length (if weighted is TRUE)
#'
#' @examples
#' strings <- c("a", "a", "a", "bc", "bc", "cdefg")
#'
#' (importance <- kwb.fakin:::sortedImportance(strings))
#'
#' # Check that each input element is mentioned in the output
#' all(unique(strings) %in% names(importance))
#'
#' # weighted = FALSE just returns the frequencies of strings in x
#' (importance <- kwb.fakin:::sortedImportance(strings, weighted = FALSE))
#'
#' # Check if the sum of frequencies is the number of elements in x
#' sum(importance) == length(strings)
#'
sortedImportance <- function(x, weighted = TRUE)
{
  freq <- table(x)

  sort(if (weighted) nchar(names(freq)) * freq else freq, decreasing = TRUE)
}

# toFrequencyData --------------------------------------------------------------
toFrequencyData <- function(freqs)
{
  sorted_frequencies <- sort(unlist(freqs), decreasing = TRUE)

  path_length <- nchar(names(sorted_frequencies))

  kwb.utils::noFactorDataFrame(
    path = names(sorted_frequencies),
    score = sorted_frequencies,
    length = path_length,
    count = sorted_frequencies / path_length,
    row.names = NULL
  )
}

# toKey ------------------------------------------------------------------------
toKey <- function(i, prefix = "p", leading.zeros = FALSE)
{
  fmt <- if (leading.zeros) {

    digits <- nchar(toKey(length(i), ""))

    paste0("%s%0", digits, "X")

  } else {

    "%s%X"
  }

  sprintf(fmt, prefix, i)
}

# rescore_and_reorder_frequency_data -------------------------------------------
rescore_and_reorder_frequency_data <-function(frequency_data, key)
{
  frequency_data$score2 <- get_frequency_score(frequency_data, key)

  row_order <- order(frequency_data$score2, decreasing = TRUE)

  kwb.utils::resetRowNames(frequency_data[row_order, ])
}

# get_frequency_score ----------------------------------------------------------
get_frequency_score <- function(frequency_data, key)
{
  key_placeholder_size <- nchar(toPlaceholder(key))

  lengths <- kwb.utils::selectColumns(frequency_data, "length")

  counts <- kwb.utils::selectColumns(frequency_data, "count")

  (lengths - key_placeholder_size) * counts
}

# toPlaceholder ----------------------------------------------------------------
toPlaceholder <- function(x)
{
  paste0("<", x, ">")
}

# printFreqs -------------------------------------------------------------------
printFreqs <- function(x, maxchar = 80)
{
  x$path <- substr(x$path, 1, maxchar)

  print(x)
}

# main_columns_winner ----------------------------------------------------------
main_columns_winner <- function()
{
  c("i", "key", "score", "count", "length")
}

# update_frequency_data_length -------------------------------------------------
update_frequency_data_length <- function(frequency_data, winner, key)
{
  winner_length <- kwb.utils::selectColumns(winner, "length")
  winner_path <- kwb.utils::selectColumns(winner, "path")

  data_length <- kwb.utils::selectColumns(frequency_data, "length")
  data_path <- kwb.utils::selectColumns(frequency_data, "path")

  shortage <- winner_length - nchar(toPlaceholder(key))

  matching <- (substr(data_path, 1, winner_length) == winner_path)

  frequency_data$length[matching] <- data_length[matching] - shortage

  frequency_data
}

# usedict ----------------------------------------------------------------------
usedict <- function(x, dict, method = "full")
{
  if (method == "full") {

    indices <- match(x, as.character(dict))

    found <- ! is.na(indices)

    x[found] <- toPlaceholder(names(dict)[indices[found]])

  } else if (method == "part") {

    # indices <- order(as.character(dict))
    keys <- names(dict)

    # for (index in indices) {
    n <- length(keys)

    for (i in seq_len(n)) {

      key <- keys[i]

      # subst <- dict[index]
      pattern <- dict[[key]]

      replacement <- toPlaceholder(key)

      # kwb.utils::printIf(TRUE, subst, "Applying substitution")

      if (i %% 20 == 0) {

        cat(sprintf(
          "%4.1f %% Substituting '%s' with '%s'...\n",
          100 * i/n, pattern, replacement
        ))
      }

      x <- gsub(pattern, replacement, x, fixed = TRUE)
      # x <- gsub(as.character(subst), toPlaceholder(names(subst)), x, fixed = TRUE)
    }

  } else {

    stop("usedict(): method must be one of 'full', 'part'", call. = FALSE)
  }

  x
}
