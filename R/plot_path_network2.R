# prepare_paths_for_network2 ---------------------------------------------------
prepare_paths_for_network2 <- function(paths)
{
  if (inherits(paths, "pathlist")) {
    return(paths)
  }

    # If a path tree is given, flatten the tree into a vector of character
  pathlist::pathlist(
    paths = if (inherits(paths, "path_tree")) flatten_tree(paths) else paths
  )
}

# get_path_network2 ------------------------------------------------------------
get_path_network2 <- function(paths, max_depth = 3, reverse = FALSE)
{
  # kwb.utils::assignPackageObjects("kwb.fakin")

  # Create data frame with each column representing a folder depth level
  folder_data <- kwb.utils::asNoFactorDataFrame(
    if (inherits(paths, "pathlist")) {
      paths@folders
    } else {
      kwb.file::to_subdir_matrix(paths, dbg = FALSE)
    }
  )

  # Reduce max_depth to the number of available columns
  max_depth <- min(
    max_depth,
    if (inherits(paths, "pathlist")) max(paths@depths) else ncol(folder_data)
  )

  # We need at least a depth of two
  stopifnot(max_depth >= 2)

  links <- do.call(rbind, lapply(2:max_depth, get_links_at_depth2, folder_data))

  node_names <- unique(unlist(links[, -3]))

  get_matching_index <- function(x) match(x, node_names) - 1

  links$source <- get_matching_index(links$source)
  links$target <- get_matching_index(links$target)

  # Swap the names of columns "source" and "target" for reverse = TRUE
  if (isTRUE(reverse)) {

    elements <- c("source", "target")

    indices <- match(elements, names(links))

    names(links)[indices] <- rev(elements)
  }

  nodes <- kwb.utils::noFactorDataFrame(
    path = node_names, name = basename(node_names)
  )

  list(links = links, nodes = nodes)
}

# get_links_at_depth2 ----------------------------------------------------------
get_links_at_depth2 <- function(i, folder_data)
{
  if (inherits(folder_data, "pathlist")) {
    source_data <- (folder_data[folder_data@depths >= i])@folders[, seq_len(i)]
    source_data <- kwb.utils::asNoFactorDataFrame(source_data)
  } else {
    # Select the first i columns
    source_data <- folder_data[, seq_len(i)]
    # Exclude rows being empty in the i-th column
    source_data <- source_data[source_data[, i] != "", ]
  }

  # Count the number of files per path
  n_files <- stats::aggregate(source_data[, 1], by = source_data, length)

  # Define helper function
  n_columns_to_path <- function(data, n) {
    kwb.utils::pasteColumns(data[, seq_len(n), drop = FALSE], sep = "/")
  }

  # Create the data frame linking source to target nodes with value as weight
  kwb.utils::noFactorDataFrame(
    source = n_columns_to_path(n_files, i - 1),
    target = n_columns_to_path(n_files, i),
    value = n_files$x
  )
}
