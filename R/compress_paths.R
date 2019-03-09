# compressPaths ----------------------------------------------------------------
compressPaths <- function(
  x, depth = 1, maxdepth = 1, dicts = list(), dbg = FALSE
)
{
  # kwb.utils::assignArgumentDefaults(compressPaths)

  kwb.utils::catIf(dbg, sprintf(
    "\n### In compressPaths(depth = %d)...\n", depth
  ))

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
      y2 <- compressPaths(
        x = as.character(dict_new), depth = depth + 1, maxdepth = maxdepth,
        dicts = dicts
      )

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

    x <- use_dictionary(x, dict)
  }

  # Create a new dictionary if there are any duplicates in x but do not
  # consider elements that are already placeholders
  dict_new <- toDictionary(x[! isPlaceholder(x)], prefix)

  x <- use_dictionary(x, dict_new)

  stopifnot(length(intersect(names(dict_new), names(dict))) == 0)

  structure(x, dict = if (extend.dict) c(dict, dict_new) else dict_new)
}

# toDictionary -----------------------------------------------------------------
toDictionary <- function(x, prefix = "a", leading.zeros = FALSE)
{
  dict <- as.list(names(sortedImportance(x)))

  structure(dict, names = toKey(seq_along(dict), prefix, leading.zeros))
}

# isPlaceholder ----------------------------------------------------------------
isPlaceholder <- function(x)
{
  grepl("^<[^<>]+>$", x)
}

# logResultIf ------------------------------------------------------------------
logResultIf <- function(dbg, x, y)
{
  if (dbg) {

    kwb.utils::catLines(c("\n### x:", x))

    kwb.utils::catLines(c("\n### y:", y))

    cat("\n### str(dict):\n")

    utils::str(kwb.utils::getAttribute(y, "dict"))
  }
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

  result <- use_dictionary(x, dict, method = "part")

  if (set.attributes) {

    attr(result, "freqs") <- freqs

    attr(result, "dict") <- dict
  }

  result
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
