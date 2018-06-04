# getSubdirsByFrequence --------------------------------------------------------
getSubdirsByFrequence <- function(subdirs, cumid, freqinfo, dbg = TRUE)
{
  kwb.utils::printIf(dbg, freqinfo)
  rows <- which(cumid[, freqinfo$depth] == freqinfo$n.x)[1]
  subdirs[rows, seq_len(freqinfo$depth)]
}

# replaceSubdirs ---------------------------------------------------------------
replaceSubdirs <- function(s, r, p)
{
  selected <- startsWithParts(s, r)
  cols <- seq(length(r) + 1, ncol(s))
  fillright <- matrix(nrow = sum(selected), ncol = length(r) -1)
  s[selected, ] <- cbind(p, s[selected, cols, drop = FALSE], fillright)

  # Remove empty columns
  maxcol <- max(which(apply(s, 2, function(x) sum(! is.na(x))) > 0))
  s[, seq_len(maxcol)]
}

# compressWithDictionary -------------------------------------------------------
compressWithDictionary <- function(subdirs, dict, fill.value = "")
{
  strings <- names(dict)[match(subdirs, as.character(dict))]

  if (! is.na(fill.value)) {
    strings[is.na(strings)] <- fill.value
  }

  matrix(strings, nrow = nrow(subdirs))
}

# removeCommonRoot -------------------------------------------------------------
#' Remove the common root parts
#'
#' @param x list of list of character as returned by
#'   \code{\link[base]{strsplit}}
#' @export
#' @examples
#' # Split paths at the slashes
#' absparts <- strsplit(c("a/b/c", "a/b/d", "a/b/e/f/g", "a/b/hi"), "/")
#'
#' # Remove the common parts of the paths
#' relparts <- removeCommonRoot(absparts)
#' relparts
#'
#' # The extracted root is returned in attribute "root"
#' attr(relparts, "root")
removeCommonRoot <- function(x)
{
  maxi <- max(sapply(x, length))

  i <- 1

  while (i < maxi && kwb.utils::allAreEqual(sapply(x, "[", i))) {
    i <- i + 1
  }

  indices <- seq_len(i - 1)

  # Determine the root path
  root <- kwb.utils::collapsed(x[[1]][indices], "/")

  # Remove the first i - 1 parts of each list entry, set attribute "root"
  structure(lapply(x, function(xx) xx[- indices]), root = root)
}

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
getFrequencies <- function(subdirs = splitPaths(paths), paths = NULL,
                           first.only = TRUE, dbg = TRUE)
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

# splitPaths -------------------------------------------------------------------
splitPaths <- function(paths, dbg = TRUE)
{
  kwb.utils::catIf(dbg, "Splitting paths... ")
  result <- strsplit(paths, "/")
  kwb.utils::catIf(dbg, "ok.\n")

  result
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
        cat(sprintf("%4.1f %% Substituting '%s' with '%s'...\n",
                  100 * i/n, pattern, replacement))
      }

      x <- gsub(pattern, replacement, x, fixed = TRUE)
      # x <- gsub(as.character(subst), toPlaceholder(names(subst)), x, fixed = TRUE)
    }

  } else {

    stop("usedict(): method must be one of 'full', 'part'", call. = FALSE)
  }

  x
}

# compressOneByOne -------------------------------------------------------------
compressOneByOne <- function(x, keys = LETTERS[1:n], n = 10)
{
  lapply(keys, function(key) {
    elapsed <- system.time(x <<- getNextLevel(x, key))
    cat(sprintf("Elapsed: %0.1f s\n", elapsed["elapsed"]))
    x
  })
}

# getNextLevel -----------------------------------------------------------------
getNextLevel <- function(x, key, set.attributes = FALSE, dbg = FALSE)
{
  freqs <- getFrequencies(paths = x, dbg = dbg)
  allfreqs <- sort(unlist(freqs), decreasing = TRUE)
  dict <- structure(list(names(allfreqs[1])), names = key)

  result <- usedict(x, dict, method = "part")

  if (set.attributes) {
    attr(result, "freqs") <- freqs
    attr(result, "dict") <- dict
  }

  result
}

# compressPaths ----------------------------------------------------------------
compressPaths <- function(x, depth = 1, maxdepth = 1, dicts = list(), dbg = FALSE)
{
  kwb.utils::catIf(dbg, sprintf("\n### In compressPaths(depth = %d)...\n", depth))

  #depth = 1; maxdepth = 3; dicts = list(); dbg = TRUE
  dirs <- dirname(x)
  ok <- (dirs != "." & dirs != "/")

  result <- x

  if (any(ok)) {

    dict_old <- if (depth > 1) dicts[[depth - 1]] else NULL

    y <- compress(x = dirs[ok], dict = dict_old, prefix = letters[depth])
    logResultIf(dbg, dirs[ok], y)

    dict_new <- kwb.utils::getAttribute(y, "dict")
    dicts[[depth]] <- dict_new
    #str(dicts)

    if (depth < maxdepth) {

      #bak <- list()
      #bak[[depth]] <- list(x, depth, dicts, dirs, ok, result, dict_old)
      #x = as.character(dict_new); depth = depth + 1
      y2 <- compressPaths(x = as.character(dict_new), depth = depth + 1,
                          maxdepth = maxdepth, dicts = dicts)
      dict_new[seq_along(dict_new)] <- as.character(y2)
      dict_new <- c(dict_new, kwb.utils::getAttribute(y2, "dict"))
    }

    result[ok] <- file.path(as.character(y), basename(x[ok]))

  } else {

    dict_new = list()
  }

  structure(result, dict = dict_new)
}

# compress ---------------------------------------------------------------------
compress <- function(x, dict = NULL, prefix = "a", extend.dict = FALSE)
{
  # If there is a dictionary replace element that are in there
  if (length(dict)) {

    x <- usedict(x, dict)
  }

  # Create a new dictionary if there are any duplicates in x but do not
  # consider elements that are already placeholders
  dict_new <- toDictionary(x[! isPlaceholder(x)], prefix)

  x <- usedict(x, dict_new)

  stopifnot(length(intersect(names(dict_new), names(dict))) == 0)

  structure(x, dict = if (extend.dict) c(dict, dict_new) else dict_new)
}

# lookup -----------------------------------------------------------------------
lookup <- function(x, dict)
{
  #x <- old_dirs; dict <- old_dict

  ready <- x %in% toPlaceholder(names(dict))

  out <- x
  out[! ready] <- toPlaceholder(names(dict[match(x[! ready], dict)]))

  out
}
