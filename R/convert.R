# toLongPath -------------------------------------------------------------------
toLongPath <- function(shortpath, dict)
{
  indices <- match(shortpath, names(dict))

  parts <- as.character(dict)[indices]

  kwb.utils::collapsed(parts, "/")
}

# toSubdirMatrix ---------------------------------------------------------------
toSubdirMatrix <- function(paths, fill.value = "", dbg = TRUE)
{
  if (! is.list(paths)) {

    paths <- splitPaths(paths, dbg = dbg)
  }

  if (! is.list(paths) || ! all(sapply(paths, mode) == "character")) {

    stop_(
      "toSubdirMatrix(): paths must be a list of character vectors ",
      "or a vector of character."
    )
  }

  # Get the maximum path depth
  max_depth <- maxdepth(paths)

  # Extend all list entries to the same length filling with ""
  extend <- function(x, length) c(x, rep(fill.value, max_depth - length(x)))

  paths <- lapply(paths, extend, max_depth)

  # Create a matrix of subdirectories
  matrix(unlist(paths), nrow = length(paths), byrow = TRUE)
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
