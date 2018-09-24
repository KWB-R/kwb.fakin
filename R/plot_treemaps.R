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
#' @param n_biggest vector of integer, specifying the number(s) of biggest
#'   folders (in terms of size and number of files) in which to "zoom into". The
#'   position in the vector represents the folder depth. For example, if
#'   \code{n_biggest = c(2, 1)}, the first element (\code{1}) indicates that
#'   sub-treemaps are produced for the two biggest subfolders below
#'   \code{root_path}: for \code{root_path/biggest-1} and for
#'   \code{root_path/biggest-2}. The second element (\code{1}) indicates that
#'   further treemaps are generated only for the biggest subfolders below
#'   \code{root_path/biggest-1} and \code{root_path/biggest-2}, respectively,
#'   in each case. The length of the vector \code{n_biggest} determines the
#'   maximal depth until which to generate treemaps. By setting an element
#'   to \code{-1L} you specify that sub-treemaps are generated for each
#'   subfolder on the corresponding folder depth.
#' @param depth current depth of recursion
#'
#' @export
#'
plot_treemaps_from_path_data <- function(
  path_data, root_path = "", name = "root", as_png = FALSE, n_levels = 3,
  output_dir = tempdir(), type = "value", args_png = list(), n_biggest = -1,
  depth = 1
)
{
  if (! is.data.frame(path_data) || nrow(path_data) == 0) {

    cat("No data frame or nothing to plot.\n")

    return()
  }

  folder_data <- kwb.utils::catAndRun(
    paste0("Preparing data for '", name, "'"),
    newLine = 3,
    prepare_for_treemap(path_data, root_path, n_keep = 0)
  )

  group_by <- names(folder_data)[seq_len(n_levels)]

  total_size <- kwb.utils::catAndRun(
    sprintf("Aggregating by first %d path levels", length(group_by)),
    aggregate_by_levels(folder_data, group_by)
  )

  n_available <- min(c(length(grep("^level", names(total_size))), n_levels))

  index <- names(total_size)[seq_len(n_available)]

  args <- list(total_size, index = index, type = type, border.col = c(
    c("darkred", rep("black", n_levels - 1))
  ))

  files <- if (as_png) {

    file.path(output_dir, sprintf(
      "treemap_%02d_%s_%s.png", depth, name, c("size", "files")
    ))
  }

  types <- c("size", "files")
  columns <- c("total_size", "n_files")
  titles <- paste("Rectangle size = total", c("size", "number of files"))
  legends <- c("Number of files", "Total size in Bytes")

  maps <- lapply(1:2, function(i) {
    #i <- 2
    map <- kwb.utils::catAndRun(
      sprintf("Creating treemap '%s'", types[i]),
      plot_one_treemap(
        file = files[i], args_png = args_png, subtitle = root_path,
        args_treemap = c(
          args, vSize = columns[i], vColor = columns[setdiff(1:2, i)],
          title = titles[i],
          title.legend = legends[i]
        )
      )
    )

    map
  })

  # If the maximum depth is not reached yet, call this function recursively for
  # the biggest subfolder in terms of size and number of files
  if (depth < length(n_biggest)) {

    biggest_folders_list <- lapply(maps, function(map) {

      #map <- maps[[1]]
      rectangles <- map$tm

      rectangles <- rectangles[rectangles$level == 1, ]

      n_select <- if (n_biggest[depth] == -1) {

        nrow(rectangles)

      } else {

        min(n_biggest[depth], nrow(rectangles))
      }

      row_order <- order(rectangles$vSize, decreasing = TRUE)

      as.character(rectangles$level_1[row_order[seq_len(n_select)]])
    })

    biggest_folders <- unique(unlist(biggest_folders_list))

    for (i in seq_along(biggest_folders)) {

      message_text <- sprintf(
        "Creating subplot %d/%d", i, length(biggest_folders)
      )

      folder <- biggest_folders[i]

      kwb.utils::catAndRun(
        message_text,
        plot_treemaps_from_path_data(
          path_data,
          root_path = paste0(check_or_set_ending_slash(root_path), folder),
          name = sprintf("%02d_%s", depth + 1, folder),
          as_png = as_png, n_levels = n_levels, output_dir = output_dir,
          type = type, args_png = args_png, depth = depth + 1
        )
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
#' @param ... further arguments passed to \code{\link{removeCommonRoot}}, such
#'   as \code{n_keep} (number of last segments to be kept from the common first
#'   part of all paths)
#'
prepare_for_treemap <- function(
  path_data, root_path = "", variable = "size", ...
)
{
  `%>%` <- magrittr::`%>%`

  # Filter for paths of type "file" and paths starting with root_path and select
  # only requested variables
  result <- path_data %>%
    filter_for_file_paths() %>%
    kwb.utils::selectColumns(c("path", variable)) %>%
    filter_for_start_path(root_path)

  # Split the paths into path segments (folder names), remove the first common
  # segments and create a data frame with the path segments in columns
  subdir_data <- result$path %>%
    splitPaths() %>%
    removeCommonRoot(...) %>%
    toSubdirMatrix(fill.value = NA) %>%
    kwb.utils::asNoFactorDataFrame()

  # Set names of path segement columns and combine with value columns
  subdir_data %>%
    stats::setNames(paste0("level_", seq_along(subdir_data))) %>%
    cbind(kwb.utils::removeColumns(result, "path"))
}

# filter_for_file_paths --------------------------------------------------------
filter_for_file_paths <- function(path_data)
{
  path_data[kwb.utils::selectColumns(path_data, "type") == "file", ]
}

# filter_for_start_path --------------------------------------------------------
filter_for_start_path <- function(path_data, start_path = "")
{
  if (start_path == "") {

    path_data

  } else {

    paths <- kwb.utils::selectColumns(path_data, "path")

    # Filter for paths starting with start_path
    path_data[left_substring_equals(paths, start_path), ]
  }
}

# aggregate_by_levels ----------------------------------------------------------
aggregate_by_levels <- function(folder_data, group_by = names(folder_data)[1:2])
{
  `%>%` <- magrittr::`%>%`

  # Convert size to numeric, otherwise we get an overflow when summing up
  folder_data$size <- as.numeric(folder_data$size)

  do.call(dplyr::group_by_, c(list(folder_data), as.list(group_by))) %>%
    dplyr::summarise_(n_files = "length(size)", total_size = "sum(size)") %>%
    as.data.frame()
}

# plot_one_treemap -------------------------------------------------------------
plot_one_treemap <- function(args_treemap, file = NULL, args_png = list(),
                             subtitle = "")
{
  if (! is.null(file)) {

    do.call(grDevices::png, c(list(file), args_png))

    on.exit(grDevices::dev.off())
  }

  map <- try(do.call(treemap::treemap, args_treemap))

  if (subtitle != "") {

    grid::grid.text(
      kwb.utils::shorten(subtitle, 100), 0.5, 0.85 * map$vpCoorY[1],
      gp = grid::gpar(col = "grey4", cex = 0.8)
    )
  }

  map
}
