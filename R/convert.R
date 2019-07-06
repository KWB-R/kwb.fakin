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
#' @keywords internal
#'
toSubdirMatrix <- function(
  paths, fill.value = "", result_type = "matrix", dbg = TRUE
)
{
  kwb.utils::warningDeprecated(
    old_name = "toSubdirMatrix", new_name = "kwb.file::to_subdir_matrix"
  )

  kwb.file::to_subdir_matrix(paths, fill.value, result_type, dbg)
}
