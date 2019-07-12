# check_depth_summaries --------------------------------------------------------
check_depth_summaries <- function(depth_summaries)
{
  depth_summary <- do.call(rbind, lapply(depth_summaries, function(df) {
    data.frame(
      n_files = sum(kwb.utils::selectColumns(df, "n_files")),
      total_size = sum(kwb.utils::selectColumns(df, "total_size"))
    )
  }))

  stopifnot(kwb.utils::allAreEqual(depth_summary$n_files))
  stopifnot(kwb.utils::allAreEqual(depth_summary$n_files))
  invisible(depth_summary)
}

# check_depth_summary ----------------------------------------------------------
check_depth_summary <- function(depth_summary)
{
  stopifnot(is.data.frame(depth_summary))
  n_col <- ncol(depth_summary)
  stopifnot(n_col > 2)
  level_names <- paste0("level_", seq_len(n_col - 2))
  expected_names <- c(level_names, "n_files", "total_size")
  stopifnot(identical(names(depth_summary), expected_names))
}

# filter_for_project -----------------------------------------------------------
filter_for_project <- function(level_data, project)
{
  is_project <- kwb.utils::selectColumns(level_data, "level_1") == project

  if (! any(is_project)) stop(
    "No project folder '", project, "' found. Available projects:\n",
    kwb.utils::stringList(unique(level_data$level_1)), call. = FALSE
  )

  kwb.utils::resetRowNames(level_data[is_project, , drop = FALSE])
}
# get_depths_from_subdir_matrix ------------------------------------------------
get_depths_from_subdir_matrix <- function(subdirs)
{
  is_na <- is.na(as.matrix(subdirs))
  max_depth <- ncol(subdirs)
  sapply(kwb.utils::asRowList(is_na), function(x) {
    if (any(x)) {
      unname(which(x)[1]) - 1
    } else {
      max_depth
    }
  })
}

# plot_files_in_depth ----------------------------------------------------------

#' Plot File Sizes over Folder Depth
#'
#' @param depth_summaries list as returned by \code{\link{get_depth_summaries}}
#' @param project name of project (in fact folder name in folder depth 1)
#' @export
#'
plot_files_in_depth <- function(depth_summaries, project)
{
  stop_("plot_files_in_depth() has been removed.")
}

