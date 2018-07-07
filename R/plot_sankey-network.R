# plot_path_network ------------------------------------------------------------

#' Plot Paths as Sankey Network
#'
#' @param paths character vector of paths
#' @param max_depth maximum depth of paths to be shown
#' @param \dots arguments passed to \code{\link[networkD3]{sankeyNetwork}}, such
#'   as \code{nodeWidth}, \code{nodePadding}, \code{fontSize}
#'
#' @return object representing an HTML page
#'
#' @export
#'
#' @examples
#' # Get the paths to all folders on the desktop
#' paths <- dir(system.file(package = "kwb.fakin"), recursive = TRUE)
#'
#' # Plot the folder network
#' plot_path_network(paths)
#'
plot_path_network <- function(paths, max_depth = 2, ...)
{
  network <- get_path_network(paths, max_depth)

  networkD3::sankeyNetwork(
    network$links, network$nodes, Source = "source", Target = "target",
    Value = "value", NodeID = "name", NodeGroup = "name", ...
  )
}

# get_path_network -------------------------------------------------------------
get_path_network <- function(paths, max_depth = 3)
{
  # kwb.utils::assignPackageObjects("kwb.fakin")

  # Create data frame with each column representing a folder depth level
  folder_data <- kwb.utils::asNoFactorDataFrame(
    toSubdirMatrix(splitPaths(paths, dbg = FALSE))
  )

  # Reduce max_depth to the number of available columns
  max_depth <- min(max_depth, ncol(folder_data))

  # We need at least a depth of two
  stopifnot(max_depth >= 2)

  links <- do.call(rbind, lapply(2:max_depth, get_links_at_depth, folder_data))

  node_names <- unique(unlist(links[, -3]))

  get_matching_index <- function(x) match(x, node_names) - 1

  links$source <- get_matching_index(links$source)
  links$target <- get_matching_index(links$target)

  nodes <- kwb.utils::noFactorDataFrame(
    path = node_names, name = basename(node_names)
  )

  list(links = links, nodes = nodes)
}

# get_links_at_depth -----------------------------------------------------------
get_links_at_depth <- function(i, folder_data)
{
  # Select the first i columns
  source_data <- folder_data[, seq_len(i)]

  # Exclude rows being empty in the i-th column
  source_data <- source_data[source_data[, i] != "", ]

  # Count the number of files per path
  n_files <- stats::aggregate(source_data[, 1], by = source_data, length)

  # Define helper function
  n_columns_to_path <- function(data, n) kwb.utils::pasteColumns(
    data[, seq_len(n), drop = FALSE], sep = "/"
  )

  # Create the data frame linking source to target nodes with value as weight
  kwb.utils::noFactorDataFrame(
    source = n_columns_to_path(n_files, i - 1),
    target = n_columns_to_path(n_files, i),
    value = n_files$x
  )
}
