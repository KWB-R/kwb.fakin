#
# author: Hauke Sonnenberg
# instructions:
#   - source the whole script to load the functions defined below
#   - go interactively through the main sections in "if (FALSE)"
#

# Provide a vector of paths ----------------------------------------------------
if (FALSE)
{
  file_info_dir <- "//medusa/processing/CONTENTS/file-info_by-department/2019-01/"
  path_list <- kwb.fakin:::read_path_information(file_info_dir, "path-info_20190101")

  # All paths together!
  paths <- unlist(lapply(path_list, kwb.utils::selectColumns, "path"))

  paths <- path_list$`path-info-ps-1_20191215_SUW_Department`$path
  paths <- path_list$`path-info-ps-1_20191215_WWT_Department`$path

  #kwb.fakin:::store(paths, "paths_to_treenodes_2")

  paths <- kwb.fakin:::restore("paths", index = 3)

  paths <- kwb.pathdict::random_paths()

  length(paths)
}

# MAIN 1: Get information on the tree leaves -----------------------------------
if (FALSE)
{
  # Split paths and put them into an efficient data structure
  system.time(leaves <- get_leaves_in_depths(paths))

  # Check if paths can be reconstructed from the new structure
  check_path_reconstruction(leaves, paths)

  # What can we use the data structure for?

  # Filter for depth levels
  depth <- 6
  p1 <- reconstruct_paths(leaves, depths = depth + 5)

  # For comparison: Use pathlist structure
  system.time(pl <- pathlist::pathlist(paths))
  p2 <- pl[pl@depths == depth]
  identical(as.character(p2), p1)
  head(p1)
  head(p2)

  # Get IDs of the parent nodes
  parent_ids <- lapply(leaves, kwb.utils::getAttribute, "parent_ids")

  # Get paths of the parent nodes
  parent_paths <- unlist(use.names = FALSE, lapply(
    leaves, kwb.utils::getAttribute, "parent_paths"
  ))

  # Get the leaves of the parent paths and check their reconstruction
  system.time(parent_leaves <- get_leaves_in_depths(parent_paths))
  check_path_reconstruction(parent_leaves, parent_paths)

  str(parent_leaves)
  head(parent_paths)

  stopifnot(anyDuplicated(parent_paths) == 0)

  length(parent_paths)
}

# MAIN 2: Get full tree information --------------------------------------------
if (FALSE)
{
  #backup <- list(nodes, edges)
  #identical(list(nodes, edges), backup)

  system.time(network <- get_nodes_and_edges(paths))

  kwb.utils::headtail(network$nodes)
  kwb.utils::headtail(network$edges)

  plot_network(network, max_depth = 5)

  subnet <- select_subtree(network, node_id = 3, n_levels = 5)

  plot_network(subnet, 100)
}

# get_leaves_in_depths ---------------------------------------------------------
get_leaves_in_depths <- function(paths, method = 1, dbg = TRUE)
{
  # Get the positions of the slashes separating folder and file names
  sep_pos <- get_separator_positions(paths, dbg = dbg)

  # Depths of the node paths a/b/c => 2 separators => node in depth 3
  node_depths <- lengths(sep_pos) + 1

  # Separator positions by depth
  original_index <- split(seq_along(paths), node_depths)

  depths <- as.integer(names(original_index))

  kwb.utils::catIf(dbg, "Extracting paths in depth   /  ")

  # Loop through the depth levels
  result <- lapply(stats::setNames(nm = depths), function(depth) {
    #depth <- depths[1]
    kwb.utils::catIf(dbg, sprintf("\b\b\b\b\b%2d/%2d", depth, max(depths)))

    in_depth <- node_depths == depth

    node_paths <- paths[in_depth]

    split_pos <- sapply(sep_pos[in_depth], "[", depth - 1)

    parents <- substr(node_paths, 1L, split_pos - 1L)
    nodes <- substr(node_paths, split_pos + 1L, nchar(node_paths))

    unique_parents <- unique(parents)

    structure(
      nodes,
      parent_ids = match(parents, unique_parents),
      parent_paths = unique_parents
    )
  })

  kwb.utils::catIf(dbg, "\n")

  structure(result, original_index = original_index, class = "leaves")
}

# get_separator_positions ------------------------------------------------------
get_separator_positions <- function(paths, dbg = TRUE)
{
  kwb.utils::catAndRun("Finding path separators", dbg = dbg, {
    sep_pos <- gregexpr("/", paths, fixed = TRUE)
  })
}

# check_path_reconstruction ----------------------------------------------------
check_path_reconstruction <- function(leaves, paths)
{
  # Reconstruct paths
  system.time(paths_reconstructed <- reconstruct_paths(leaves))

  # Was the reconstruction successful?
  identical(paths, paths_reconstructed)
}

# reconstruct_paths ------------------------------------------------------------
reconstruct_paths <- function(x, depths = NULL)
{
  #x <- leaves;depths = NULL
  stopifnot(inherits(x, "leaves"))

  depths <- kwb.utils::defaultIfNULL(depths, as.integer(names(x)))
  depth_names <- as.character(depths)

  is_invalid <- ! depth_names %in% names(x)

  if (any(is_invalid)) {
    stop(call. = FALSE, sprintf(
      "There are no paths in depth(s): %s.\nAvailable depths: %s",
      paste(depths[is_invalid], collapse = ", "),
      paste(names(x), collapse = ", ")
    ))
  }

  x_depths <- x[depth_names]

  paths_by_depth <- lapply(x_depths, function(xx) {
    ids <- kwb.utils::getAttribute(xx, "parent_ids")
    paste(kwb.utils::getAttribute(xx, "parent_paths")[ids], xx, sep = "/")
  })

  indices_list <- kwb.utils::getAttribute(x, "original_index")
  indices <- order(unlist(indices_list[depth_names], use.names = FALSE))

  unlist(paths_by_depth, use.names = FALSE)[indices]
}

# get_nodes_and_edges ----------------------------------------------------------
get_nodes_and_edges <- function(
  paths, depths, result_type = "data.frames", dbg = TRUE
)
{
  stopifnot(is.character(paths))
  stopifnot(result_type %in% c("lists", "data.frames"))

  kwb.utils::catIf(dbg, "Number of paths:", length(paths), "\n")

  # Remove "//" at the beginning to avoid empty fields after splittig
  paths <- kwb.utils::catAndRun("Removing '//' at the beginning", dbg = dbg, {
    gsub("^//(?=[^/])", "", paths, perl = TRUE)
  })

  # Get the positions of the slashes separating folder and file names
  sep_pos <- get_separator_positions(paths, dbg = dbg)

  # Maximal path depth (= number of slashes + 1)
  max_depth <- max(lengths(sep_pos)) + 1L

  offset <- 0
  nodes <- list()
  edges <- list()

  parents <- character(length(paths))

  kwb.utils::catIf(dbg, "Analysing nodes in depth   /  ")

  for (depth in seq_len(max_depth)) {

    #depth <- 2
    kwb.utils::catIf(dbg, sprintf("\b\b\b\b\b%2d/%2d", depth, max_depth))

    in_depth <- depth <= lengths(sep_pos) + 1

    pos <- sep_pos[in_depth]

    is_leaf <- lengths(pos) < depth

    split_pos <- integer(sum(in_depth))
    split_pos[is_leaf] <- nchar(paths[in_depth][is_leaf]) + 1
    split_pos[! is_leaf] <- unlist(lapply(pos[! is_leaf], "[", depth))

    stopifnot(all(split_pos > 0))

    subdirs <- substr(paths[in_depth], 1L, split_pos - 1L)

    node_paths <- unique(subdirs)

    parent_ids <- if (depth > 1) {
      unname(nodes[[depth - 1]][parents[in_depth][! duplicated(subdirs)]])
    } # else NULL

    stopifnot(all(! is.na(parent_ids)))

    parents[in_depth] <- subdirs

    ids <- seq_along(node_paths) + offset

    offset <- offset + length(node_paths)

    nodes[[depth]] <- stats::setNames(ids, node_paths)

    edges[[depth]] <- if (depth > 1) {
      cbind(node = ids, parent = parent_ids)
    } # else NULL
  }

  kwb.utils::catIf(dbg, "\n")

  # If requested, convert lists "nodes" and "edges" to data frames
  if (result_type == "data.frames") {

    node_ids <- unlist(nodes)

    nodes <- kwb.utils::noFactorDataFrame(
      id = as.integer(node_ids),
      depth = rep.int(seq_along(nodes), lengths(nodes)),
      name = sapply(strsplit(names(node_ids), "/", fixed = TRUE), tail, 1),
      path = names(node_ids)
    )

    edges <- kwb.utils::rbindAll(
      edges, nameColumn = "depth", namesAsFactor = FALSE
    )
  }

  list(nodes = nodes, edges = edges)
}

# select_subtree ---------------------------------------------------------------
select_subtree <- function(network, node_id, n_levels = 2L, dbg = TRUE)
{
  #node_id=40;n_levels=2L
  nodes <- kwb.utils::selectElements(network, "nodes")
  edges <- kwb.utils::selectElements(network, "edges")

  node_ids <- kwb.utils::selectColumns(nodes, "id")

  node_index <- which(node_ids == node_id)

  if (length(node_index) == 0) {
    stop("No such node: ", node_id)
  }

  node_depth <- kwb.utils::selectColumns(nodes, "depth")[node_index]

  # We are only interested in edges to nodes that are deeper than node_depth
  child_depths <- kwb.utils::selectColumns(edges, "depth")

  min_depth <- node_depth + 1L
  max_depth <- node_depth + n_levels

  edges <- edges[kwb.utils::inRange(child_depths, min_depth, max_depth), ]

  # Collect the ids of all child nodes
  ids <- node_id

  result_ids <- list()

  while (length(ids)) {

    kwb.utils::catAndRun(paste("Adding", length(ids), "ids"), dbg = dbg, {

      result_ids[[length(result_ids) + 1L]] <- ids

      ids <- edges$node[edges$parent %in% ids]
    })
  }

  result_ids <- unlist(result_ids)

  result_nodes <- nodes[nodes$id %in% result_ids, ]
  result_edges <- edges[edges$node %in% result_ids, ]

  stopifnot(anyDuplicated(result_nodes$id) == 0)

  # Renumber edge nodes
  result_edges[, 1:2] <- match(as.matrix(result_edges[, 1:2]), result_nodes$id)

  # Renumber nodes
  result_nodes$id <- seq_len(nrow(result_nodes))

  # Return list of new nodes and new edges
  list(nodes = result_nodes, edges = result_edges)
}

# plot_network -----------------------------------------------------------------
plot_network <- function(network, max_depth = 6)
{
  nodes <- network$nodes[network$nodes$depth <= max_depth, ]
  edges <- network$edges[network$edges$depth <= max_depth, ]

  graph <- igraph::make_graph(
    as.integer(t(as.matrix(edges[, c("parent", "node")]))),
    directed = FALSE
  )

  igraph::plot.igraph(graph, vertex.size = 3, vertex.label = "")
}
