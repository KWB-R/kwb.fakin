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
  # Create matrix with each column representing a folder depth level
  folder_matrix <- toSubdirMatrix(splitPaths(paths))

  # Convert to data frame
  folder_data <- as_no_factor_data_frame(folder_matrix)

  # Reduce max_depth to the number of available columns
  max_depth <- min(max_depth, ncol(folder_matrix))

  # We need at least a depth of two
  stopifnot(max_depth >= 2)

  # Define helper function
  columns_to_path <- function(...) kwb.utils::pasteColumns(..., sep = "/")

  links <- do.call(rbind, lapply(2:max_depth, function(i) {

    column_indices <- seq_len(i)
    column_names <- paste0("V", column_indices)

    base_path_data <- stats::setNames(folder_data[, column_indices], column_names)

    files_per_path <- stats::aggregate(
      rep(1, nrow(base_path_data)),
      by = kwb.utils::selectColumns(base_path_data, column_names, drop = FALSE),
      FUN = length
    )

    # Exclude rows being empty in the i-th column
    files_per_path <- files_per_path[files_per_path[, i] != "", ]

    # Create the data frame linking source to target nodes with value as weight
    no_factor_data_frame(
      source = columns_to_path(files_per_path, column_names[seq_len(i - 1)]),
      target = columns_to_path(files_per_path, column_names),
      value = files_per_path[, i + 1]
    )
  }))

  node_names <- unique(unlist(links[, -3]))

  get_matching_index <- function(x) match(x, node_names) - 1

  links$source <- get_matching_index(links$source)
  links$target <- get_matching_index(links$target)

  nodes <- no_factor_data_frame(path = node_names, name = basename(node_names))

  list(links = links, nodes = nodes)
}

# as_no_factor_data_frame ------------------------------------------------------
as_no_factor_data_frame <- function(...)
{
  as.data.frame(..., stringsAsFactors = FALSE)
}

# no_factor_data_frame ---------------------------------------------------------
no_factor_data_frame <- function(...)
{
  data.frame(..., stringsAsFactors = FALSE)
}
