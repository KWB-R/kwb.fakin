# get_depth_summaries ----------------------------------------------------------

#' Get File Number and Size Summary per Folder Depth
#'
#' @param file_data data frame as returned by \code{\link{read_file_info}}
#' @param project_dir path by which to filter the paths in \code{file_data},
#'   passed to \code{fakin.path.app:::prepare_for_treemap}
#' @param max_depth maximum depth for which to calculate a summary. If
#'   \code{NULL} (default), all summaries are created for all available path
#'   depths
#' @return list of data frames
#' @export
#'
get_depth_summaries <- function(file_data, project_dir, max_depth = NULL)
{
  level_data <- fakin.path.app:::prepare_for_treemap(file_data, project_dir)

  level_data_to_depth_summaries(level_data, max_depth = max_depth)
}

# level_data_to_depth_summaries ------------------------------------------------
level_data_to_depth_summaries <- function(level_data, max_depth = NULL)
{
  check_level_data(level_data)

  max_depth <- kwb.utils::defaultIfNULL(
    max_depth, sum(grepl("^level_", names(level_data)))
  )

  lapply(seq_len(max_depth), function(depth) {
    message("depth = ", depth)
    columns <- paste0("level_", seq_len(depth))
    fakin.path.app:::aggregate_by_levels(level_data, columns)
  })
}

# check_level_data -------------------------------------------------------------
check_level_data <- function(level_data)
{
  stopifnot(is.data.frame(level_data))
  columns <- c(paste0("level_", seq_len(ncol(level_data) - 1)), "size")
  kwb.utils::checkForMissingColumns(level_data, columns)
  stopifnot(identical(names(level_data), columns))
}
