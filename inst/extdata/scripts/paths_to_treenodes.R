# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # Provide a matrix of subdirectory names
  # - Either: from real paths
  file_info_dir <- "//medusa/processing/CONTENTS/file-info_by-department/2019-12/"
  path_list <- kwb.fakin:::read_path_information(file_info_dir)
  pl <- pathlist::pathlist(path_list$`path-info-ps-1_20191201_SUW_Department`$path)
  x <- pl@folders

  # - Or: from random paths
  x <- kwb.file::to_subdir_matrix(kwb.pathdict:::random_paths(max_depth = 5))

  # Number of paths
  nrow(x)

  # Create a network structure (with elements nodes and edges)
  system.time(network1 <- create_network(x))
  system.time(network2 <- create_network2(x))

  identical(network1, network2)

  #diffobj::diffStr(network1, network2)

  net_2 <- prune_network(network, depth = 2)
  net_3 <- prune_network(network, depth = 3)

  str(net_2)
  str(net_3)

  net <- net_2

  graph <- igraph::make_graph(t(net$edges), directed = FALSE)

  plot(
    graph, vertex.color = "red", vertex.size = 2, vertex.label.cex = 0.6
    , vertex.label = net$nodes$name[unique(as.integer(edges))]
    , layout = igraph::layout.kamada.kawai
  )

  testtable <- data.frame(path = as.character(pl)[1:5000], value = 1)

  system.time(tree1 <- data.tree::FromDataFrameTable(testtable, pathName = "path"))
  system.time(tree2 <- FromDataFrameTable(testtable, pathName = "path"))

  identical(as.data.frame(tree1), as.data.frame(tree2))

  plot(tree1)
  plot(tree2)

  data.tree::as.Node(as.data.frame(matrix(nrow = 2, byrow = TRUE, c(
    "A", "B", "C.txt",
    "A", "D", "E.txt"
  ))))
}

# Find nodes -------------------------------------------------------------------
if (FALSE)
{
  x <- pl@folders
  x <- kwb.file::to_subdir_matrix(kwb.pathdict:::random_paths(max_depth = 5))
  dim(x)

  system.time(node_pos2 <- get_node_positions(x))
  identical(node_pos, node_pos2)

  t(node_pos)
  x
}

# get_node_positions -----------------------------------------------------------
get_node_positions <- function(x)
{
  stopifnot(is.matrix(x), nrow(x) > 0)

  next_equals <- x[-nrow(x), ] == x[-1, ]

  n_col <- ncol(x)

  row_of_false <- matrix(nrow = 1, rep(FALSE, n_col))

  next_equals <- rbind(row_of_false, next_equals)

  do.call(rbind, lapply(seq_len(n_col), function(j) {

    message(j, "/", n_col)

    cols <- next_equals[, seq_len(j), drop = FALSE]

    i <- which(! Reduce("&", kwb.utils::asColumnList(cols)))

    matrix(c(i, rep(j, length(i))), ncol = 2)
  }))
}

# create_network ---------------------------------------------------------------
create_network <- function(x)
{
  kwb.utils::stopIfNotMatrix(x)

  # Which fields are set, i.e. not empty?
  is_set <- matrix(nzchar(x), nrow = nrow(x), ncol = ncol(x))

  # Initialise a matrix of (tree node) IDs
  ids <- matrix(integer(), nrow = nrow(x), ncol = ncol(x))

  # Maximal ID so far
  max_id <- 0

  # Node names occurring in the different path depths
  node_names <- list()

  # Loop through the columns of the subdirectory matrix
  for (j in seq_len(ncol(x))) {

    #j <- 1
    message("Giving IDs to nodes in depth ", j, "/", ncol(x))

    rows <- which(is_set[, j])

    xx <- x[rows, j]

    temp_ids <- generate_ids(xx)

    if (j > 1) {
      temp_ids <- generate_ids(xx = cbind(ids[rows, seq_len(j - 1)], temp_ids))
    }

    node_names[[j]] <- sapply(unname(split(xx, temp_ids)), "[", 1)

    ids[rows, j] <- temp_ids + max_id

    max_id <- max_id + max(temp_ids)
  }

  to_nodes_and_edges(ids, is_set, node_names)
}

# create_network2 --------------------------------------------------------------
create_network2 <- function(x)
{
  kwb.utils::stopIfNotMatrix(x)

  # Which fields are set, i.e. not empty?
  is_set <- matrix(nzchar(x), nrow = nrow(x), ncol = ncol(x))

  # Initialise a matrix of (tree node) IDs
  ids <- matrix(integer(), nrow = nrow(x), ncol = ncol(x))

  # Maximal ID so far
  max_id <- 0

  # Node names occurring in the different path depths
  node_names <- list()

  # Loop through the columns of the subdirectory matrix
  for (j in seq(1, ncol(x))) {

    #j <- 2
    message("Giving IDs to nodes in depth ", j, "/", ncol(x))

    rows <- which(is_set[, j])

    # Values in the current column j
    xj <- x[rows, j]

    # Prepend node IDs of parents unless this is the first column
    xx <- if (j == 1) {

      xj

    } else {

      paste(ids[rows, j - 1], xj, sep = "-")
    }

    # Give new IDs to unique combinations of parent (if any) and value
    unique_values <- unique(xx)

    new_ids <- match(xx, unique_values)

    node_names[[j]] <- sapply(unname(split(xj, new_ids)), "[", 1)

    ids[rows, j] <- new_ids + max_id

    max_id <- max_id + length(unique_values)
  }

  # Generate edges between start-node and end-node
  to_nodes_and_edges(ids, is_set, node_names)
}

# to_nodes_and_edges -----------------------------------------------------------
to_nodes_and_edges <- function(ids, is_set, node_names)
{
  `%>%` <- magrittr::`%>%`

  n_col <- ncol(ids)

  # Generate edges between start-node and end-node
  edges <- do.call(rbind, lapply(seq(2, n_col, by = 1), function(j) {
    message("Creating edges to depth ", j, "/", n_col)
    unique(ids[is_set[, j], c(j - 1, j)])
  }))

  nodes <- seq_along(node_names) %>%
    lapply(function(i) kwb.utils::noFactorDataFrame(name = node_names[[i]])) %>%
    kwb.utils::rbindAll("depth", namesAsFactor = FALSE)

  list(nodes = nodes, edges = edges)
}

# generate_ids -----------------------------------------------------------------
# Generate unique IDs
generate_ids <- function(xx)
{
  if (is.matrix(xx)) {
    xx <- do.call(paste, c(kwb.utils::asColumnList(xx), sep = "-"))
  }

  match(xx, unique(xx))
}

# prune_network ----------------------------------------------------------------
prune_network <- function(network, depth = 2)
{
  node_ids <- which(network$nodes$depth <= depth)

  network$edges <- network$edges[network$edges[, 2] %in% node_ids, ]

  network
}
