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
  #remotes::install_github("kwb-r/kwb.file@dev")

  # Provide a matrix of subdirectory names
  #system.time(subdirs <- kwb.file::to_subdir_matrix(paths))
  # Number of rows = number of paths, number of columns = max depth level
  #dim(subdirs)
  # Convert matrix back to paths
  # paths <- subdir_matrix_to_paths(subdirs)

  # Create a network structure (with elements nodes and edges)
  system.time(network <- get_nodes_and_edges(paths))

  #diffobj::diffStr(network_1, network_2)
  net_1 <- prune_network(network)

  # Prune the network at different maximal depths
  nets <- lapply(2:4, function(depth) prune_network(network, depth = depth))

  graphs <- lapply(nets, network_to_igraph)

  plot(graphs[[2]])

  plot(
    graphs[[2]], vertex.color = "red", vertex.size = 2, vertex.label.cex = 0.6
  )

  igraph::as_data_frame(graphs[[2]], what = "vertices")

  testtable <- data.frame(path = paths[1:15], value = 1)

  system.time(tree <- data.tree::FromDataFrameTable(testtable, pathName = "path"))

  plot(tree)
}

# prune_network ----------------------------------------------------------------
prune_network <- function(network, depth = 2)
{
  nodes <- kwb.utils::selectElements(network, "nodes")
  edges <- kwb.utils::selectElements(network, "edges")

  edge_nodes <- kwb.utils::selectColumns(edges, "node")

  node_ids <- which(kwb.utils::selectColumns(nodes, "depth") <= depth)

  network$edges <- edges[edge_nodes %in% node_ids, , drop = FALSE]

  network
}

# network_to_igraph ------------------------------------------------------------
network_to_igraph <- function(net)
{
  #net <- nets[[1]]

  edges <- as.integer(as.matrix(kwb.utils::selectElements(net, "edges")))
  nodes <- kwb.utils::selectElements(net, "nodes")[sort(unique(edges)), ]

  node_names <- kwb.utils::selectColumns(nodes, "name")

  g <- igraph::make_undirected_graph(edges)

  igraph::set_vertex_attr(g, "name", value = node_names)
}
