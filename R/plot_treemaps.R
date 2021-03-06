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
#'   \code{n_biggest = c(2, 1)}, the first element (\code{2}) indicates that
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
#' @param types type(s) of treeplots: one or both of \code{c("size", "files")}
#'   (the default).
#' @export
#'
plot_treemaps_from_path_data <- function(
  path_data, root_path = "", name = "root", as_png = FALSE, n_levels = 3,
  output_dir = tempdir(), type = "value", args_png = list(), n_biggest = -1,
  depth = 1, types = c("size", "files")
)
{
  #kwb.utils::assignArgumentDefaults(kwb.fakin:::plot_treemaps_from_path_data)
  #kwb.utils::assignPackageObjects("kwb.fakin")

  if (! inherits(path_data, "pathlist") && ! check_path_data(path_data)) {
    return()
  }

  total_size <- if (inherits(path_data, "pathlist")) {
    prepare_for_n_level_treemap2(path_data, n_levels, root_path, name)
  } else {
    prepare_for_n_level_treemap(path_data, n_levels, root_path, name)
  }

  # Store the root that was returned in attribute "root"
  root <- kwb.utils::getAttribute(total_size, "root")

  n_available <- min(c(length(grep("^level", names(total_size))), n_levels))

  index <- names(total_size)[seq_len(n_available)]

  settings <- default_treeplot_settings()

  files <- default_treemap_files(as_png, settings, output_dir, depth, name)

  maps <- lapply(types, function(map_type) {

    map <- kwb.utils::catAndRun(
      sprintf("Creating treemap '%s'", map_type),
      kwb.utils::callWith(
        plot_treemap,
        dtf = total_size,
        args_treemap(
          index = index,
          type = type,
          n_levels = n_levels,
          settings = settings,
          area_represents = map_type
        ),
        file = unname(files[map_type]),
        args_png = args_png,
        subtitle = sprintf("Root folder: %s", root)
      )
    )

    map
  })

  # If the maximum depth is not reached yet, call this function recursively for
  # the biggest subfolder in terms of size and number of files
  if (depth < length(n_biggest)) {

    biggest <- unique(unlist(lapply(
      maps, get_biggest_folders_from_map, n = n_biggest[depth]
    )))

    for (i in seq_along(biggest)) {

      kwb.utils::catAndRun(
        messageText = sprintf("Creating subplot %d/%d", i, length(biggest)),
        plot_treemaps_from_path_data(
          path_data,
          root_path = paste0(check_or_set_ending_slash(root_path), biggest[i]),
          name = sprintf("%02d_%s", depth + 1, biggest[i]),
          as_png = as_png, n_levels = n_levels, output_dir = output_dir,
          type = type, args_png = args_png, depth = depth + 1,
          types = types
        )
      )
    }
  }

  files
}

# check_path_data --------------------------------------------------------------
check_path_data <- function(path_data)
{
  if (! is.data.frame(path_data) || nrow(path_data) == 0) {
    cat("No data frame or nothing to plot.\n")
    FALSE
  } else {
    TRUE
  }
}

# prepare_for_n_level_treemap --------------------------------------------------
prepare_for_n_level_treemap <- function(
  path_data, n_levels = 1L, root_path = "", name = "root"
)
{
  folder_data <- kwb.utils::catAndRun(
    sprintf("Preparing data for '%s'", name), newLine = 3,
    prepare_for_treemap(path_data, root_path, n_keep = 1L)
  )

  group_by <- names(folder_data)[seq_len(n_levels)]

  result <- kwb.utils::catAndRun(
    sprintf("Aggregating by first %d path levels", length(group_by)),
    aggregate_by_levels(folder_data, group_by)
  )

  # Return the root path in attribute "root"
  structure(result, root = kwb.utils::getAttribute(folder_data, "root"))
}

# prepare_for_n_level_treemap2 -------------------------------------------------
prepare_for_n_level_treemap2 <- function(
  path_list, n_levels = 1L, root_path = "", name = "root", variable = "size"
)
{
  #n_levels = 1L; root_path = ""; name = "root"; variable = "size"
  stopifnot(inherits(path_list, "pathlist"))

  kwb.utils::checkForMissingColumns(path_list@data, c("type", variable))

  folder_data <- kwb.utils::catAndRun(
    sprintf("Preparing data for '%s'", name), newLine = 3, {

      # Filter for paths of type "file" by ec
      pl <- path_list[path_list@data$type == "file"]

      # Select only required extra columns
      pl@data <- kwb.utils::selectColumns(pl@data, variable, drop = FALSE)

      # Filter for paths starting with root_path if root_path is given
      if (root_path != "") {
        pl <- pl[left_substring_equals(as.character(pl), root_path)]
      }

      # Let pathlist remove common roots
      pl <- pathlist::pathlist(segments = pathlist::as.list(pl), data = pl@data)
      subdir_data <- kwb.utils::asNoFactorDataFrame(pl@folders)

      # Set names of path segment columns and combine with value columns
      subdir_data %>%
        stats::setNames(paste0("level_", seq_along(subdir_data))) %>%
        cbind(pl@data)
    }
  )

  group_by <- names(folder_data)[seq_len(n_levels)]

  result <- kwb.utils::catAndRun(
    sprintf("Aggregating by first %d path levels", length(group_by)),
    aggregate_by_levels(folder_data, group_by)
  )

  # Return the root path in attribute "root"
  structure(result, root = pl@root)
}

# args_treemap -----------------------------------------------------------------
args_treemap <- function(
  index = "level_1", type = "value", n_levels = 1,
  border.col = c("darkred", rep("black", n_levels - 1)),
  settings = default_treeplot_settings(), area_represents = "size"
)
{
  anti_representation <- setdiff(names(settings), area_represents)
  setting <- kwb.utils::selectElements(settings, area_represents)
  anti_setting <- kwb.utils::selectElements(settings, anti_representation)

  list(
    index = index,
    type = type,
    border.col = border.col,
    vSize = setting$column,
    vColor = anti_setting$column,
    title = setting$title,
    title.legend = setting$legend
  )
}

# default_treemap_files --------------------------------------------------------
default_treemap_files <- function(as_png, settings, output_dir, depth, name)
{
  if (as_png) {

    sapply(stats::setNames(nm = names(settings)), function(type) {
      file.path(output_dir, sprintf(settings[[type]]$filename, depth, name))
    })
  }
}

# default_treeplot_settings ----------------------------------------------------
default_treeplot_settings <- function()
{
  list(
    size = list(
      column = "total_size",
      title = "Rectangle size = total size",
      legend = "Number of files",
      filename = "treemap_%02d_%s_size.png"
    ),
    files = list(
      column = "n_files",
      title = "Rectangle size = total number of files",
      legend = "Total size in Bytes",
      filename = "treemap_%02d_%s_files.png"
    )
  )
}

# get_biggest_folders_from_map -------------------------------------------------
get_biggest_folders_from_map <- function(map, n)
{
  #map <- maps[[1]]
  rectangles <- map$tm

  rectangles <- rectangles[rectangles$level == 1, ]

  n_select <- if (n == -1) {

    nrow(rectangles)

  } else {

    min(n, nrow(rectangles))
  }

  row_order <- order(rectangles$vSize, decreasing = TRUE)

  as.character(rectangles$level_1[row_order[seq_len(n_select)]])
}

# prepare_for_treemap ----------------------------------------------------------

#' Prepare and Filter Path Data for Treemap Plot
#'
#' @param path_data data frame as returned by \code{\link{read_file_info}}
#' @param root_path path to the folder that contains all paths to be considered.
#'   By setting the root path to "/path/to/root" you can "zoom into" the
#'   treeplot, showing the contents below "/path/to/root" only. If
#'   \code{root_path} is \code{""} (default) all paths in \code{path_data} are
#'   considered.
#' @param variable name(s) of variable(s) to be selected. Default: "size"
#' @param ... further arguments passed to
#'   \code{\link[kwb.file]{remove_common_root}}, such as \code{n_keep} (number
#'   of last segments to be kept from the common first part of all paths)
prepare_for_treemap <- function(
  path_data, root_path = "", variable = "size", ...
)
{
  kwb.utils::checkForMissingColumns(path_data, c("type", "path", variable))

  # Filter for paths of type "file" and paths starting with root_path and select
  # only requested variables
  result <- path_data %>%
    exclude_directories() %>%
    kwb.utils::selectColumns(c("path", variable)) %>%
    filter_for_start_path(root_path)

  # Split the paths into path segments (folder names), remove the first common
  # segments and create a data frame with the path segments in columns
  paths <- result$path %>%
    kwb.file::split_paths() %>%
    kwb.file::remove_common_root(...)

  # Store the root that was returned by remove_common_root() in attribute "root"
  root <- kwb.utils::getAttribute(paths, "root")

  # Create data frame of subdirectories
  subdir_data <- paths %>%
    kwb.file::to_subdir_matrix(fill.value = NA) %>%
    kwb.utils::asNoFactorDataFrame()

  # Set names of path segement columns and combine with value columns
  result <- subdir_data %>%
    stats::setNames(paste0("level_", seq_along(subdir_data))) %>%
    cbind(kwb.utils::removeColumns(result, "path"))

  # Return the common root in attribute "root"
  structure(result, root = root)
}

# exclude_directories ----------------------------------------------------------
exclude_directories <- function(path_data)
{
  types <- kwb.utils::selectColumns(path_data, "type")
  path_data[types == "file", ]
}

# filter_for_start_path --------------------------------------------------------
filter_for_start_path <- function(path_data, start_path = "")
{
  if (start_path == "") {
    return(path_data)
  }

  paths <- kwb.utils::selectColumns(path_data, "path")

  # Filter for paths starting with start_path
  path_data[left_substring_equals(paths, start_path), ]
}

# aggregate_by_levels ----------------------------------------------------------
aggregate_by_levels <- function(folder_data, group_by = names(folder_data)[1:2])
{
  # Convert size to numeric, otherwise we get an overflow when summing up
  folder_data$size <- as.numeric(folder_data$size)

  do.call(dplyr::group_by_, c(list(folder_data), as.list(group_by))) %>%
    dplyr::summarise_(n_files = "length(size)", total_size = "sum(size)") %>%
    as.data.frame()
}

# plot_treemap -----------------------------------------------------------------
plot_treemap <- function(
  dtf, ..., file = NULL, args_png = list(), subtitle = ""
)
{
  #args <- list()
  args <- list(...)

  if (length(args) == 0) {
    args <- args_treemap()
  }

  if (! is.null(file)) {
    do.call(grDevices::png, c(list(file), args_png))
    on.exit(grDevices::dev.off())
  }

  map <- try(kwb.utils::callWith(treemap::treemap, dtf = dtf, args))

  if (subtitle != "") {

    grid::grid.text(
      kwb.utils::shorten(subtitle, 100), 0.5, 0.85 * map$vpCoorY[1],
      gp = grid::gpar(col = "grey4", cex = 0.8)
    )
  }

  map
}
