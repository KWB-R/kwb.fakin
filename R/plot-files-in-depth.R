# plot_files_in_depth ----------------------------------------------------------

#' Plot File Sizes over Folder Depth
#'
#' @param depth_summaries list as returned by \code{\link{get_depth_summaries}}
#' @param project name of project (in fact folder name in folder depth 1)
#' @export
#'
plot_files_in_depth <- function(depth_summaries, project)
{
  # Filter each depth summary by the project
  depth_summaries <- lapply(depth_summaries, filter_for_project, project)

  # Check depth summaries
  check_depth_summaries(depth_summaries)

  # Get the most detailed summary available
  max_depth_summary <- depth_summaries[[length(depth_summaries)]]
  stopifnot(all(kwb.utils::selectColumns(max_depth_summary, "n_files") == 1))

  # Convert to "size_in_depth" data
  size_in_depth <- depth_summary_to_size_in_depth(max_depth_summary)

  plot_size_in_depth(size_in_depth) +
    ggplot2::ggtitle(paste("Project:", project)) +
    ggplot2::xlab("Folder depth") +
    ggplot2::ylab("File size in MB")
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

# depth_summary_to_size_in_depth -----------------------------------------------
depth_summary_to_size_in_depth <- function(depth_summary)
{
  check_depth_summary(depth_summary)
  level_columns <- setdiff(names(depth_summary), c("n_files", "total_size"))
  subdirs <- kwb.utils::selectColumns(depth_summary, level_columns)
  data.frame(
    depth = get_depths_from_subdir_matrix(subdirs),
    size = kwb.utils::selectColumns(depth_summary, c("total_size"))
  )
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

# plot_size_in_depth -----------------------------------------------------------
plot_size_in_depth <- function(size_in_depth)
{
  bytes_per_mb <- 1024 * 1024
  kwb.utils::checkForMissingColumns(size_in_depth, c("depth", "size"))
  size_in_depth$size_MB <- size_in_depth$size / bytes_per_mb
  ggplot2::ggplot(size_in_depth, ggplot2::aes_string("depth", "size_MB")) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(breaks = 1:100) +
    ggplot2::scale_y_log10()
}
