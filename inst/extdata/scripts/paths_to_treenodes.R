# Provide a vector of paths ----------------------------------------------------
if (FALSE)
{
  # - Either: from real paths
  file_info_dir <- "//medusa/processing/CONTENTS/file-info_by-department/2019-12/"
  path_list <- kwb.fakin:::read_path_information(file_info_dir)
  paths <- path_list$`path-info-ps-1_20191201_SUW_Department`$path
  #kwb.fakin:::store(paths, "paths_to_treenodes")
  #paths <- kwb.fakin:::restore("paths", index = 1)

  # - Or: simple test paths
  paths <- c("a/b/a", "a/b/c", "a/c/a", "a/b/a/x")

  # - Or: random paths consisting of english words
  paths <- kwb.pathdict:::random_paths(max_depth = 5)

  length(paths)
}

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # Provide a matrix of subdirectory names
  #remotes::install_github("kwb-r/kwb.file@dev")
  system.time(subdirs_1 <- kwb.file::to_subdir_matrix(paths, method = 1))
  system.time(subdirs_2 <- kwb.file::to_subdir_matrix(paths, method = 2))

  # Check the subdirectory matrices for identity
  identical(subdirs_1, subdirs_2)

  # Continue with either subdirectory matrix
  subdirs <- subdirs_1

  # Number of rows = number of paths, number of columns = max depth level
  dim(subdirs)

  # Create a network structure (with elements nodes and edges)
  system.time(network_1 <- create_network(subdirs, method = 1))
  system.time(network_2 <- create_network(subdirs, method = 2))

  identical(network_1, network_2)
  #diffobj::diffStr(network_1, network_2)

  # Continue with either network
  network <- network_1

  # Prune the network at different maximal depths
  nets <- lapply(2:5, function(depth) prune_network(network, depth = depth))

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

  testtable <- data.frame(path = paths[1:20], value = 1)

  system.time(tree <- data.tree::FromDataFrameTable(testtable, pathName = "path"))

  plot(tree)
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
  nodes <- kwb.utils::selectElements(network, "nodes")
  edges <- kwb.utils::selectElements(network, "edges")

  stopifnot(is.matrix(edges), ncol(edges) >= 2)

  node_ids <- which(kwb.utils::selectColumns(nodes, "depth") <= depth)

  network$edges <- edges[edges[, 2] %in% node_ids, ]

  network
}
