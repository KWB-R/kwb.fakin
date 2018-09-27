# toLongPath -------------------------------------------------------------------
toLongPath <- function(shortpath, dict)
{
  indices <- match(shortpath, names(dict))

  parts <- as.character(dict)[indices]

  kwb.utils::collapsed(parts, "/")
}

# toSubdirMatrix ---------------------------------------------------------------

#' Convert a Vector of Paths to a Matrix of Subfolders
#'
#' @param paths vector of path strings
#' @param fill.value value used to fill empty cells of the result matrix
#' @param result_type one of \code{c("matrix", "data.frame")}, specifying the
#'   type of object to be returned
#' @param dbg if \code{TRUE} debug messages are shown
#'
#' @return matrix or data frame, depending on \code{result_type}
#'
#' @export
#'
#' @examples
#' folder_matrix <- kwb.fakin:::toSubdirMatrix(c("a1/b1/c1", "a1/b2", "a2"))
#'
#' folder_matrix
#'
#' dim(folder_matrix)
#'
#' folder_matrix[folder_matrix[, 1] == "a1", ]
#'
toSubdirMatrix <- function(
  paths, fill.value = "", result_type = "matrix", dbg = TRUE
)
{
  stopifnot(result_type %in% c("matrix", "data.frame"))

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

  # Extend all list entries to a length of <max_depth>, filling with <fill.with>
  paths <- lapply(paths, kwb.utils::enlargeVector, max_depth, fill.value)

  # Create a matrix of subdirectories
  result <- matrix(unlist(paths), nrow = length(paths), byrow = TRUE)

  # Return the result as an object of the requested type
  if (result_type == "matrix") {

    result

  } else if (result_type == "data.frame") {

    kwb.utils::asNoFactorDataFrame(result)

  } else {

    stop_("result_type not supported: ", result_type)
  }
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
