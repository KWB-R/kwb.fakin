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
get_path_network2 <- function(
  paths, max_depth = 3, reverse = FALSE,
  weight_by = c("n_files", "size", "none")[1], sizes = NULL
)
{
  # kwb.utils::assignPackageObjects("kwb.fakin")

  is_pathlist <- inherits(paths, "pathlist")

  # Create data frame with each column representing a folder depth level
  folder_data <- kwb.utils::asNoFactorDataFrame(
    if (is_pathlist) {
      paths@folders
    } else {
      kwb.file::to_subdir_matrix(paths, dbg = FALSE)
    }
  )

  # Reduce max_depth to the number of available columns
  max_depth <- min(
    max_depth,
    if (is_pathlist) max(paths@depths) else ncol(folder_data)
  )

  # We need at least a depth of two
  stopifnot(max_depth >= 2)

  links <- do.call(rbind, lapply(
    2:max_depth, get_links_at_depth2, folder_data, weight_by = weight_by,
    sizes = sizes
  ))

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
get_links_at_depth2 <- function(
  i, folder_data, weight_by = c("n_files", "size", "none")[1], sizes = NULL
)
{
  stopifnot(weight_by %in% c("n_files", "size", "none"))

  is_pathlist <- inherits(folder_data, "pathlist")

  if (is_pathlist) {

    is_deeper <- folder_data@depths >= i
    folder_data <- folder_data[is_deeper]
    source_data <- folder_data@folders[, seq_len(i)]
    source_data <- kwb.utils::asNoFactorDataFrame(source_data)

  } else {

    # Select the first i columns
    source_data <- folder_data[, seq_len(i)]
    # Exclude rows being empty in the i-th column
    is_deeper <- source_data[, i] != ""
    source_data <- source_data[is_deeper, ]
  }

  sizes <- sizes[is_deeper]
  source_data$size <- kwb.utils::defaultIfNULL(sizes, 1)
  n_levels <- ncol(source_data) - 1

  # Count the number of files per path
  FUN <- list(n_files = length, size = sum, none = function(x) 1)[[weight_by]]

  stats <- stats::aggregate(
    x = source_data$size,
    by = kwb.utils::removeColumns(source_data, "size"),
    FUN = FUN
  )

  # Define helper function
  n_columns_to_path <- function(data, n) {
    kwb.utils::pasteColumns(data[, seq_len(n), drop = FALSE], sep = "/")
  }

  # Create the data frame linking source to target nodes with value as weight
  kwb.utils::noFactorDataFrame(
    source = n_columns_to_path(stats, i - 1),
    target = n_columns_to_path(stats, i),
    value = stats$x
  )
}
