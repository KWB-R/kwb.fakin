# splitPaths -------------------------------------------------------------------
splitPaths <- function(paths, dbg = TRUE)
{
  kwb.utils::catAndRun("Splitting paths", dbg = dbg, strsplit(paths, "/"))
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

#' Do Subfolder List Elements Start with Given Folder Names?
#'
#' @param parts list of list of character as returned by
#'   \code{kwb.fakin:::splitPahts} or a matrix of character representing the
#'   subfolder names at the different folder depths as returned by
#'    \code{\link{toSubdirMatrix}}.
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
#' startsWithParts(parts, elements = c("a", "b"))
#' startsWithParts(parts, elements = c("b", "c"))
#'
#' subdir_matrix <- kwb.fakin::toSubdirMatrix(parts)
#' startsWithParts(subdir_matrix, elements = c("a", "b"))
#' startsWithParts(subdir_matrix, elements = c("b", "c"))
#'
startsWithParts <- function(parts, elements)
{
  stopifnot(is.list(parts) || is.matrix(parts))

  stopifnot(all(! is.na(elements)))

  length_out <- if (is.list(parts)) length(parts) else nrow(parts)

  selected_at_level <- lapply(seq_along(elements), function(i) {

    if (is.list(parts)) {

      sapply(parts, "[", i) == elements[i]

    } else {

      ! is.na(parts[, i]) & (parts[, i] == elements[i])
    }
  })

  Reduce(`&`, selected_at_level, init = rep(TRUE, length_out))
}

# removeCommonRoot -------------------------------------------------------------

#' Remove the common root parts
#'
#' @param x list of vectors of character as returned by
#'   \code{\link[base]{strsplit}} or a vector of character.
#' @param n_keep number of common path segments to keep (so that the path
#'   tree keeps its root)
#' @param dbg if \code{TRUE} debug messages are shown
#'
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
#'
removeCommonRoot <- function(x, n_keep = 0, dbg = TRUE)
{
  if (! (was_list <- is.list(x))) {

    x <- splitPaths(as.character(x), dbg = dbg)
  }

  root <- ""

  n_common <- get_common_start_segments(x)

  if ((n_remove <- n_common - n_keep) > 0) {

    # Determine the root path
    root <- kwb.utils::collapsed(x[[1]][1:n_remove], "/")

    # Remove the first n_common parts of each list entry
    text <- paste("Removing the first", n_remove, "path segments")

    kwb.utils::catAndRun(text, dbg = dbg, {

      x <- lapply(x, function(segments) {

        if (length(segments) > n_remove) segments[- (1:n_remove)] else ""
      })
    })
  }

  # If the input was not a list, convert the list back to a vector of character
  if (! was_list) {

    kwb.utils::catAndRun("Putting path segments together", dbg = dbg, {

      x <- sapply(x, function(xx) do.call(paste, c(as.list(xx), sep = "/")))
    })
  }

  # Set attribute "root"
  structure(x, root = root)
}

# get_common_start_segments ----------------------------------------------------
get_common_start_segments <- function(list_of_segments)
{
  # Define helper function
  get_segment <- function(depth) {

    result <- sapply(list_of_segments, "[", depth)
    result[is.na(result)] <- ""
    result
  }

  tree_height <- maxdepth(parts = list_of_segments)

  i <- 1

  while (i < tree_height && kwb.utils::allAreEqual(get_segment(i))) {

    i <- i + 1
  }

  i - 1
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
