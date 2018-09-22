# plot_all_treemaps ------------------------------------------------------------

#' Plot Treemaps for All given Path Infos
#'
#' @param path_infos list of data frames each of which contains file path
#'   information as returned by \code{\link{read_file_info}}
#' @param as_png if \code{TRUE} the plots are saved to png-files in
#'   \code{tempdir()}. The name is then taken from the names of the elements in
#'   \code{path_infos}. Otherwise the plot go into the current graphical device.
#' @param \dots further arguments passed to
#'   \code{\link{plot_treemaps_from_path_data}}, such as \code{n_levels}
#' @return for \code{as_png = TRUE} vector of paths to the created png files.
#'
#' @export
#'
plot_all_treemaps <- function(path_infos, as_png = TRUE, ...)
{
  if (! is.list(path_infos) || ! all(sapply(path_infos, is.data.frame))) {

    stop_("path_data_list must be a list of data frames.")
  }

  if (any(kwb.utils::is.unnamed(path_infos))) {

    stop_(sprintf(
      "The list '%s' given to plot_all_treemaps must be named",
      deparse(substitute(path_infos))
    ))
  }

  lapply(names(path_infos), function(name) {

    plot_treemaps_from_path_data(
      path_data = path_infos[[name]],
      name = name,
      as_png = as_png,
      ...
    )
  })
}

# plot_treemaps_from_path_data -------------------------------------------------

#' Plot Treemaps Given File Path Data
#'
#' @param path_data data frame containing file path information as returned by
#'   \code{\link{read_file_info}}
#' @param root_path path to the folder that contains all paths to be considered.
#'   By setting the root path to "/path/to/root" you can "zoom into" the
#'   treeplot, showing the contents below "/path/to/root" only. If
#'   \code{root_path} is \code{""} (default) all paths in \code{path_data} are
#'   considered.
#' @param name name to be used in png file name if \code{as_png} is set. If
#'   \code{path_data} is a list, the names of the list elements are used.
#' @param as_png if \code{TRUE} (default) the plots are saved to png files in
#'   the directory given in \code{output_dir} (\code{tempdir()} by default).
#'   Otherwise they are plotted into the active graphical device.
#' @param n_levels number of folder depth levels to be shown in the plots
#' @param output_dir path to output directory if \code{as_png = TRUE}. Default:
#'   \code{tempdir()}
#' @param type passed to \code{\link[treemap]{treemap}}
#' @param args_png list of arguments passed to \code{\link[grDevices]{png}} if
#'   \code{as_png = TRUE}
#' @param max_depth maximum depth until which to generate sub-treemaps
#' @param n_biggest number of biggest folders (in terms of size and number of
#'   files) for which to generate sub-treeplots.
#' @param depth current depth of recursion
#'
#' @export
#'
plot_treemaps_from_path_data <- function(
  path_data, root_path = "", name = "root", as_png = FALSE, n_levels = 3,
  output_dir = tempdir(), type = "value", args_png = list(), max_depth = 1,
  n_biggest = 1, depth = 1
)
{
  kwb.utils::catAndRun(
    paste0("Preparing data for '", name, "'"),
    newLine = 3,
    folder_data <- prepare_for_treemap(path_data, root_path, n_keep = 1)
  )

  # Convert size to numeric, otherwise we get an overflow when summing up
  folder_data$size <- as.numeric(folder_data$size)

  group_by <- names(folder_data)[seq_len(n_levels)]

  kwb.utils::catAndRun(
    "Aggregating by first path levels",
    total_size <- aggregate_by_levels(folder_data, group_by)
  )

  index <- names(total_size)[seq_len(n_levels)]

  args <- list(total_size, index = index, type = type)

  files <- if (as_png) {

    filenames <- sprintf("treemap_%s_%s.png", name, c("size", "files"))

    files <- file.path(output_dir, filenames)
  }

  treemaps <- list()

  kwb.utils::catAndRun(
    "Creating treemap 'size'",
    treemaps$size <- plot_one_treemap(
      file = files[1], args_png = args_png, args_treemap = c(
        args, vSize = "total_size", vColor = "n_files",
        title = "Rectangle size = total size",
        title.legend = "Number of files"
      )
    )
  )

  kwb.utils::catAndRun(
    "Creating treemap 'files'",
    treemaps$files <- plot_one_treemap(
      file = files[2],
      args_png = args_png, args_treemap = c(
        args, vSize = "n_files", vColor = "total_size",
        title = "Rectangle size = total number of files",
        title.legend = "Total size in Bytes"
      )
    )
  )

  # If the maximum depth is not reached yet, call this function recursively for
  # the biggest subfolder in terms of size and number of files
  if (depth < max_depth) {

    biggest_folders <- lapply(treemaps, function(treemap) {

      rectangles <- treemap$tm

      stopifnot(sum(rectangles$level == 1) == 1)

      rectangles <- rectangles[rectangles$level == 2, ]

      row_order <- order(rectangles$vSize, decreasing = TRUE)

      as.character(rectangles$level_2[row_order[seq_len(n_biggest)]])
    })

    for (folder in unique(unlist(biggest_folders))) {

      cat("Recursively create plots for subfolder:", folder, "\n")

      plot_treemaps_from_path_data(
        path_data,
        root_path = paste0(check_or_set_ending_slash(root_path), folder),
        name = sprintf("%02d_%s", depth + 1, folder),
        as_png = as_png, n_levels = n_levels, output_dir = output_dir,
        type = type, args_png = args_png, max_depth = max_depth,
        depth = depth + 1
      )
    }
  }

  files
}

# prepare_for_treemap ------------------------------------------------------------

#' Prepare and Filter Path Data for Treemap Plot
#'
#' @param path_data data frame as returned by \code{\link{read_file_info}}
#' @param root_path path to the folder that contains all paths to be considered.
#'   By setting the root path to "/path/to/root" you can "zoom into" the
#'   treeplot, showing the contents below "/path/to/root" only. If
#'   \code{root_path} is \code{""} (default) all paths in \code{path_data} are
#'   considered.
#' @param variable name(s) of variable(s) to be selected. Default: "size"
#' @param path_type file path_type(s) to filter for. Default: "file"
#' @param ... further arguments passed to \code{\link{removeCommonRoot}}, such
#'   as \code{n_keep} (number of last segments to be kept from the common first
#'   part of all paths)
#'
prepare_for_treemap <- function(
  path_data, root_path = "", variable = "size", path_type = "file", ...
)
{
  `%>%` <- magrittr::`%>%`

  # Compare the path types with the requested type(s)
  has_type <- kwb.utils::selectColumns(path_data, "type") %in% path_type

  # Select only requested variables
  result <- kwb.utils::selectColumns(path_data, c("path", variable))

  # Keep only paths of the requested type
  result <- result[has_type, ]

  # If a root_path is given, filter for paths starting with this path
  if (root_path != "") {

    result <- result[substr(result$path, 1, nchar(root_path)) == root_path, ]
  }

  # Convert paths to path segment matrix ignoring the first common parts
  path_segment_data <- result$path %>%
    removeCommonRoot(...) %>%
    toSubdirMatrix(fill.value = NA) %>%
    kwb.utils::asNoFactorDataFrame()

  # Set names of path segement columns and combine with value columns
  path_segment_data %>%
    stats::setNames(paste0("level_", seq_along(path_segment_data))) %>%
    cbind(kwb.utils::removeColumns(result, "path"))
}

# aggregate_by_levels ----------------------------------------------------------
aggregate_by_levels <- function(folder_data, group_by = names(folder_data)[1:2])
{
  `%>%` <- magrittr::`%>%`

  do.call(dplyr::group_by_, c(list(folder_data), as.list(group_by))) %>%
    dplyr::summarise_(n_files = "length(size)", total_size = "sum(size)") %>%
    as.data.frame()
}

# plot_one_treemap -------------------------------------------------------------
plot_one_treemap <- function(args_treemap, file = NULL, args_png = list())
{
  if (! is.null(file)) {

    do.call(grDevices::png, c(list(file), args_png))

    on.exit(grDevices::dev.off())
  }

  do.call(treemap::treemap, args_treemap)
}
