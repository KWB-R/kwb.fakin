# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # Provide a matrix of subdirectory names
  # - Either: from real paths
  file_info_dir <- "//medusa/processing/CONTENTS/file-info_by-department/2019-12/"
  path_list <- kwb.fakin:::read_path_information(file_info_dir)
  paths <- path_list$`path-info-ps-1_20191201_SUW_Department`$path
  pl <- pathlist::pathlist(paths)
  x <- pl@folders

  # - Or: test paths
  paths <- c("a/b/a", "a/b/c", "a/c/a", "a/b/a/x")

  # - Or: from random paths
  paths <- kwb.pathdict:::random_paths(max_depth = 5)

  system.time(x1 <- kwb.file::to_subdir_matrix(paths, method = 1))
  system.time(x2 <- kwb.file::to_subdir_matrix(paths, method = 2))

  identical(x1, x2)
  x <- x1

  # Number of paths
  nrow(x)

  # Create a network structure (with elements nodes and edges)
  system.time(network <- create_network(x))
  system.time(network2 <- create_network(x, method = 2))
  identical(network, network2)
  #diffobj::diffStr(network, network2)

  nets <- lapply(2:4, function(depth) prune_network(network, depth = depth))

  graphs <- lapply(nets, function(net) {
    igraph::make_graph(t(net$edges), directed = FALSE)
  })

  for (i in seq_along(graphs)) {
    net <- nets[[i]]
    plot(
      graphs[[i]],
      vertex.color = "red",
      vertex.size = 2,
      vertex.label.cex = 0.6
      , vertex.label = net$nodes$name[unique(as.integer(net$edges))]
      , layout = igraph::layout.kamada.kawai
    )
  }

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
create_network <- function(x, method = 1)
{
  kwb.utils::stopIfNotMatrix(x)

  # Initialise a matrix of (tree node) IDs
  ids <- matrix(integer(), nrow = nrow(x), ncol = ncol(x))

  # Maximal ID so far
  max_id <- 0

  # Node names occurring in the different path depths
  node_names <- list()

  # Loop through the columns of the subdirectory matrix
  for (j in seq_len(ncol(x))) {

    #j <- 3
    message("Giving IDs to nodes in depth ", j, "/", ncol(x))

    # Which rows have non-empty values in the current column?
    rows <- which(nzchar(x[, j]))

    # Values in the current column j
    xj <- x[rows, j]

    # Prepend node IDs of parents unless this is the first column
    xx <- if (j == 1) xj else paste(ids[rows, j - 1], xj, sep = "-")

    # Give new IDs to unique combinations of parent (if any) and value
    new_ids <- match(xx, unique(xx))

    # Store the node names in the list
    node_names[[j]] <- xj[diff(c(0, new_ids)) > 0]

    # Store the new IDs in the ID matrix
    ids[rows, j] <- new_ids + max_id

    # Update maximal ID
    max_id <- max_id + max(new_ids)
  }

  # Generate edges between start-node and end-node
  to_nodes_and_edges(ids, node_names)
}

# to_nodes_and_edges -----------------------------------------------------------
to_nodes_and_edges <- function(ids, node_names)
{
  n_col <- ncol(ids)

  # Generate edges between start-node and end-node
  edges <- do.call(rbind, lapply(seq.int(2L, n_col, by = 1L), function(j) {
    message("Creating edges to depth ", j, "/", n_col)
    unique(ids[! is.na(ids[, j]), c(j - 1L, j)])
  }))

  nodes <- kwb.utils::rbindAll(
    x = lapply(node_names, function(name) {
      data.frame(name = name, stringsAsFactors = FALSE)
    }),
    nameColumn = "depth",
    namesAsFactor = FALSE
  )

  list(nodes = nodes, edges = edges)
}

# prune_network ----------------------------------------------------------------
prune_network <- function(network, depth = 2)
{
  node_ids <- which(network$nodes$depth <= depth)

  network$edges <- network$edges[network$edges[, 2] %in% node_ids, ]

  network
}
