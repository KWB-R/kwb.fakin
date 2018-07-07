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
  folder_matrix <- toSubdirMatrix(splitPaths(paths))

  #folder_matrix <- cbind("<root>", folder_matrix)

  # Reduce max_depth to the number of columns
  max_depth <- min(max_depth, ncol(folder_matrix))

  stopifnot(max_depth >= 2)

  link_list <- lapply(2:max_depth, function(i) {

    base_path_data <- as.data.frame(
      folder_matrix[, seq_len(i)],
      stringsAsFactors = FALSE
    )

    names(base_path_data) <- paste0("V", seq_len(i))

    columns <- paste0("V", seq_len(i))

    files_per_base_path <- stats::aggregate(
      rep(1, nrow(base_path_data)),
      by = kwb.utils::selectColumns(base_path_data, columns, drop = FALSE),
      FUN = length
    )

    files_per_base_path <- files_per_base_path[files_per_base_path[, i] != "", ]

    paste_to_path <- function(x) {

      kwb.utils::pasteColumns(sep = "/", as.data.frame(
        x, stringsAsFactors = FALSE
      ))
    }

    data.frame(
      source = paste_to_path(files_per_base_path[, seq_len(i - 1)]),
      target = paste_to_path(files_per_base_path[, seq_len(i)]),
      value = files_per_base_path[, i + 1],
      stringsAsFactors = FALSE
    )
  })

  links <- do.call(rbind, link_list)

  node_names <- unique(unlist(links[, -3]))

  get_matching_index <- function(x) match(x, node_names) - 1

  links$source <- get_matching_index(links$source)
  links$target <- get_matching_index(links$target)

  nodes <- data.frame(
    path = node_names,
    name = basename(node_names),
    stringsAsFactors = FALSE
  )

  list(links = links, nodes = nodes)
}
