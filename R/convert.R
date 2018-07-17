# toLongPath -------------------------------------------------------------------
toLongPath <- function(shortpath, dict)
{
  indices <- match(shortpath, names(dict))

  parts <- as.character(dict)[indices]

  kwb.utils::collapsed(parts, "/")
}

# toSubdirMatrix ---------------------------------------------------------------
toSubdirMatrix <- function(dirparts, fill.value = "", dbg = TRUE)
{
  if (! is.list(dirparts)) {

    dirparts <- splitPaths(dirparts, dbg = dbg)
  }

  if (! is.list(dirparts) || ! all(sapply(dirparts, mode) == "character")) {

    stop_("toSubdirMatrix(): dirparts must be a list of character vectors!")
  }

  # Get the maximum path depth
  maxdepth <- maxdepth(dirparts)

  # Extend all list entries to the same length filling with ""
  extend <- function(x, length) c(x, rep(fill.value, maxdepth - length(x)))

  dirparts <- lapply(dirparts, extend, maxdepth)

  # Create a matrix of subdirectories
  matrix(unlist(dirparts), nrow = length(dirparts), byrow = TRUE)
}

# toCumulativeID ---------------------------------------------------------------
toCumulativeID <- function(subdirs)
{
  cumpaths <- matrix(nrow = nrow(subdirs), ncol = ncol(subdirs))

  cumids <- cumpaths

  cat("depth: 00")

  for (depth in seq_len(ncol(subdirs))) {

    cat(sprintf("\b\b%2d", depth))

    reached <- ! is.na(subdirs[, depth])

    cumpaths[reached, depth] <- if (depth > 1) {

      paste0(cumpaths[reached, depth - 1], subdirs[reached, depth])

    } else {

      subdirs[reached, depth]
    }

    cumids[, depth] <- as.integer(as.factor(cumpaths[, depth]))
  }

  cat("\n")

  cumids
}
