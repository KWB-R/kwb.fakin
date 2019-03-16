# toLongPath -------------------------------------------------------------------
toLongPath <- function(shortpath, dict)
{
  indices <- match(shortpath, names(dict))

  parts <- as.character(dict)[indices]

  kwb.utils::collapsed(parts, "/")
}

# toSubdirMatrix ---------------------------------------------------------------

#' Deprecated.
#'
#' This function is deprecated. Please use kwb.file::to_subdir_matrix() instead.
#'
#' @param paths vector of path strings
#' @param fill.value value used to fill empty cells of the result matrix
#' @param result_type one of \code{c("matrix", "data.frame")}, specifying the
#'   type of object to be returned
#' @param dbg if \code{TRUE} debug messages are shown
#' @return matrix or data frame, depending on \code{result_type}
#' @export
toSubdirMatrix <- function(
  paths, fill.value = "", result_type = "matrix", dbg = TRUE
)
{
  warning(
    "Please use kwb.file::to_subdir_matrix() instead of toSubdirMatrix()!",
    call. = FALSE
  )

  kwb.file::to_subdir_matrix(paths, fill.value, result_type, dbg)
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
