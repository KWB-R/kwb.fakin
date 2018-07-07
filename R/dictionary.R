# getDictOneByOne --------------------------------------------------------------

#' Get a Path Dictionary
#'
#' @param paths vector of character strings representing file or folder paths
#' @param n number of compression levels
#'
getDictOneByOne <- function(paths, n = 10)
{
  # Get the frequencies of the directory paths
  freqs <- getFrequencies(paths = paths, first.only = TRUE)

  Freqs0 <- toFrequencyData(freqs)

  dict <- list()

  Freqs <- Freqs0

  while (length(dict) < n) {

    # Next key to be used in the dictionary
    key <- toKey(length(dict) + 1)

    # Rescore and reorder Freqs
    Freqs$score2 <- (Freqs$length - nchar(toPlaceholder(key))) * Freqs$count

    rank <- order(Freqs$score2, decreasing = TRUE)

    Freqs <- kwb.utils::resetRowNames(Freqs[rank, ])

    printFreqs(Freqs)

    # Which is the "winning" path?
    winner <- Freqs[1, ]

    # Remove the winning path
    Freqs <- Freqs[-1, ]

    # Put the winning path into the dictionary
    dict[[key]] <- winner$path

    winner <- cbind(i = length(dict), key = key, winner)

    columns <- c("i", "key", "score", "count", "length")

    winner <- kwb.utils::moveColumnsToFront(winner, columns)

    print(winner, row.names = FALSE)

    # Update the length (reduce by "shortage", the number of saved characters)
    shortage <- winner$length - nchar(toPlaceholder(key))

    matching <- (substr(Freqs$path, 1, winner$length) == winner$path)

    Freqs[matching, "length"] <- Freqs[matching, "length"] - shortage
  }

  dict
}

# getFrequencies ---------------------------------------------------------------
getFrequencies <- function(
  subdirs = splitPaths(paths), paths = NULL, first.only = TRUE, dbg = TRUE
)
{
  n.levels <- sapply(subdirs, length)

  if (dbg) {

    main <- "Distribution of path depths"

    graphics::hist(n.levels, main = main)

    kwb.utils::printIf(dbg, table(n.levels), main)
  }

  lapply(seq_len(max(n.levels)), function(i) {

    isLongEnough <- n.levels >= i

    kwb.utils::catIf(dbg, sprintf("i = %d, n = %d...\n", i, sum(isLongEnough)))

    x <- sapply(subdirs[isLongEnough], function(xx) {

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
  f <- sort(unlist(freqs), decreasing = TRUE)

  pathlen <- nchar(names(f))

  count <- f / pathlen

  data.frame(
    path = names(f),
    score = f,
    length = pathlen,
    count = count,
    stringsAsFactors = FALSE,
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
