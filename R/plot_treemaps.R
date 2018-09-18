# plot_all_treemaps ------------------------------------------------------------

#' Plot Treemaps for All given Path Infos
#'
#' @param path_infos list of data frames containing file path information as
#'   they are returned by \code{\link{read_file_info}}
#' @param as_png if \code{TRUE} (default) the plots are saved to png files in
#'   \code{tempdir()}. Otherwise they are plotted into the active graphical
#'   device.
#'
#' @export
#'
plot_all_treemaps <- function(path_infos, as_png = TRUE)
{
  lapply(names(path_infos), function(root_name) {

    kwb.utils::catAndRun(paste("Plotting", root_name), {

      path_data <- prepare_path_data(path_infos[[root_name]])

      folder_size <- get_first_path_segments_and_size(path_data, 4)

      total_size <- aggregate_by_first_three_levels(folder_size)

      png_name <- if (as_png) root_name else ""

      plot_two_treemaps(total_size, png_name = png_name)
    })
  })
}

# prepare_path_data ------------------------------------------------------------
prepare_path_data <- function(path_info)
{
  # Prepare data frame for data.tree
  path_data <- kwb.utils::selectColumns(path_info, c("path", "size", "type"))

  # Keep only files
  path_data <- path_data[path_data$type == "file", ]

  # Cut off the first segments of the common root path
  path_data$path <- kwb.fakin::removeCommonRoot(path_data$path, n_keep = 1)

  path_data
}

# get_first_path_segments_and_size ---------------------------------------------
get_first_path_segments_and_size <- function(path_data, n_levels = 3)
{
  path_segments <- toSubdirMatrix(path_data$path)

  stopifnot(kwb.utils::inRange(n_levels, 1, ncol(path_segments)))

  cbind(
    kwb.utils::asNoFactorDataFrame(path_segments[, 1:n_levels]),
    size = as.numeric(kwb.utils::selectColumns(path_data, "size"))
  )
}

# aggregate_by_first_three_levels ----------------------------------------------
aggregate_by_first_three_levels <- function(folder_size)
{
  `%>%` <- magrittr::`%>%`

  folder_size %>%
    dplyr::group_by_("V1", "V2", "V3") %>%
    dplyr::summarise_(
      n_files = "length(size)",
      total_size = "sum(size)"
    ) %>%
    as.data.frame()
}

# plot_two_treemaps ------------------------------------------------------------
plot_two_treemaps <- function(total_size, png_name = "", output_dir = tempdir())
{
  args <- list(total_size, index = c("V2", "V3"), type = "value")

  files <- if (png_name != "") {

    filenames <- sprintf("treemap_%s_%s.png", png_name, c("size", "files"))

    file.path(output_dir, filenames)
  }

  plot_treemap(file = files[1], args_treemap = c(
    args, vSize = "total_size", vColor = "n_files",
    title = "Rectangle size = total size",
    title.legend = "Number of files"
  ))

  plot_treemap(file = files[2], args_treemap = c(
    args, vSize = "n_files", vColor = "total_size",
    title = "Rectangle size = total number of files",
    title.legend = "Total size in Bytes"
  ))

  files
}

# plot_treemap -----------------------------------------------------------------
plot_treemap <- function(args_treemap, file = NULL, args_png = list())
{
  if (! is.null(file)) {

    do.call(grDevices::png, c(list(file), args_png))

    on.exit(grDevices::dev.off())
  }

  do.call(treemap::treemap, args_treemap)
}
