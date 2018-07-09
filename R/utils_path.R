# splitPaths -------------------------------------------------------------------
splitPaths <- function(paths, dbg = TRUE)
{
  kwb.utils::catIf(dbg, "Splitting paths... ")

  result <- strsplit(paths, "/")

  kwb.utils::catIf(dbg, "ok.\n")

  result
}

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

# startsWithParts --------------------------------------------------------------

#' Does a strsplit list start with given sequence of elements?
#'
#' @param parts list of list of character as returned by \code{strsplit}
#' @param elements vector of character giving the sequence of strings to be
#'   found in \code{parts}
#' @return vector of logical as long as \code{parts} containing \code{TRUE} at
#'   positions \code{i} for which \code{all(parts[[i]][seq_along(elements)] ==
#'   elements)} is \code{TRUE}
#'
#' @export
#'
#' @examples
#' parts <- strsplit(c("a/b/c", "a/b/d", "b/c"), "/")
#' startsWithParts(parts, c("a", "b"))
#' startsWithParts(parts, c("b", "c"))
#'
startsWithParts <- function(parts, elements)
{
  stopifnot(is.list(parts) || is.matrix(parts))
  stopifnot(all(! is.na(elements)))

  indices <- seq_along(elements)

  if (is.list(parts)) {

    selected <- rep(TRUE, length(parts))

    for (i in indices) {

      selected <- selected & sapply(parts, "[", i) == elements[i]
    }

  } else {

    selected <- rep(TRUE, nrow(parts))

    for (i in seq_along(elements)) {

      selected <- selected & ! is.na(parts[, i]) & (parts[, i] == elements[i])
    }
  }

  selected
}

# removeCommonRoot -------------------------------------------------------------

#' Remove the common root parts
#'
#' @param x list of vectors of character as returned by
#'   \code{\link[base]{strsplit}} or a vector of character.
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
  if (is.list(x)) {

    was_list <- TRUE

  } else {

    was_list <- FALSE

    x <- splitPaths(as.character(x))
  }

  maxi <- max(sapply(x, length))

  i <- 1

  while (i < maxi && kwb.utils::allAreEqual(sapply(x, "[", i))) {

    i <- i + 1
  }

  indices <- seq_len(i - 1)

  # Determine the root path
  root <- kwb.utils::collapsed(x[[1]][indices], "/")

  # Remove the first i - 1 parts of each list entry, set attribute "root"
  result <- structure(lapply(x, function(xx) xx[- indices]), root = root)

  # If the input was not a list, convert the list back to a vector of character
  if (! was_list) {

    sapply(result, function(x) do.call(file.path, as.list(x)))

  } else {

    result
  }
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
