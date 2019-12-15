# M A I N ----------------------------------------------------------------------
if (FALSE)
{
  paths <- kwb.fakin:::restore("paths")
  paths <- kwb.pathdict::random_paths()

  length(paths)

  # For comparison: pathlist structure
  system.time(pl <- pathlist::pathlist(paths))

  # Split paths and put them into an efficient data structure
  system.time(nodes_in_depth <- get_nodes_in_depth_from_paths(paths))

  # Reconstruct paths
  system.time(paths_reconstructed <- reconstruct_paths(nodes_in_depth))

  # Was the reconstruction successful?
  identical(paths, paths_reconstructed)

  # What can we use the data structure for?

  # Filter for depth levels
  depth <- 4
  p1 <- reconstruct_paths(nodes_in_depth, depths = depth + 1)
  p2 <- pl[pl@depths == depth]
  identical(as.character(p2), p1)
  head(p1)
  head(p2)

  lapply(nodes_in_depth, kwb.utils::getAttribute, "parent_ids")

  parent_ids <- lapply(nodes_in_depth, kwb.utils::getAttribute, "parent_ids")

  parent_paths <- unlist(use.names = FALSE, lapply(
    nodes_in_depth, kwb.utils::getAttribute, "parent_paths"
  ))

  system.time(parents_in_depth <- get_nodes_in_depth_from_paths(parent_paths))
  parent_paths_reconstructed <- reconstruct_paths(parents_in_depth)
  identical(parent_paths, parent_paths_reconstructed)

  str(parents_in_depth)

  head(parent_paths)

  stopifnot(anyDuplicated(parent_paths) == 0)

  length(parent_paths)

  max_ids <- sapply(parent_ids, max)

  lapply(seq_along(parent_ids), function(i) {
    parent_ids[[i]] + max_ids[i]
  })

  n_parents <- unname(sapply(parent_ids, max))

  offsets <- rep(n_parents[-1], lengths(parent_ids)[-length(parent_ids)])

  nodes <- cbind(
    depth = rep(as.integer(names(parent_ids)) - 1L, lengths(parent_ids)),
    id = unlist(parent_ids, use.names = FALSE)
  )

  nrow(nodes)
  length(offsets)

  head(paths)

  depth_name <- "2"
  nodes_in_depth[[depth_name]]

  ids <- rep.int(sapply(result, max)[-length(result)], lengths(result[-1]))
}

# MAIN 2 -----------------------------------------------------------------------
if (FALSE)
{
  paths <- gsub("^//(?=[^/])", "", kwb.fakin:::restore("paths"), perl = TRUE)

  head(paths)

  nodes_in_depth_old <- get_nodes_in_depth_from_paths(paths)

  node_depths <- kwb.utils::getAttribute(nodes_in_depth_old, "node_depths")
  sep_pos <- kwb.utils::getAttribute(nodes_in_depth_old, "sep_pos")

  depths <- sort(unique(node_depths))

  dbg <- TRUE

  offset <- 0
  nodes_in_depth <- list()
  links_in_depth <- list()

  system.time(for (depth in seq_len(max(depths))) {

    #depth <- 2
    kwb.utils::catIf(dbg, sprintf("\b\b\b\b\b%2d/%2d", depth, max(depths)))

    in_depth <- depth <= lengths(sep_pos)
    #sum(in_depth)

    if (depth > 1) {

      split_pos <- unlist(lapply(sep_pos[in_depth], "[", depth -c(1L, 0L)))
      subdirs <- substr(rep(paths[in_depth], each = 2L), 1L, split_pos - 1L)
      m <- matrix(subdirs, ncol = 2, byrow = TRUE)
      m <- m[which(! duplicated(m[, 2])), , drop = FALSE]
      parent_ids <- unname(nodes_in_depth[[depth - 1]][m[, 1]])
      node_paths <- m[, 2]

    } else {

      split_pos <- unlist(lapply(sep_pos[in_depth], "[", depth))
      subdirs <- substr(paths[in_depth], 1L, split_pos - 1L)
      node_paths <- unique(subdirs)
    }

    ids <- seq_along(node_paths) + offset

    offset <- offset + length(node_paths)

    nodes_in_depth[[depth]] <- stats::setNames(ids, node_paths)

    if (depth > 1) {
      links_in_depth[[depth]] <- cbind(node = ids, parent = parent_ids)
    }
  })
}

# get_nodes_in_depth_from_paths ------------------------------------------------
get_nodes_in_depth_from_paths <- function(paths, dbg = TRUE)
{
  kwb.utils::catAndRun("Find path separators", dbg = dbg, {
    sep_pos <- gregexpr("/", paths, fixed = TRUE)
  })

  # Depths of the node paths a/b/c => 2 separators => node in depth 3
  node_depths <- lengths(sep_pos) + 1

  # Separator positions by depth
  index_by_depth <- split(seq_along(paths), node_depths)

  depths <- as.integer(names(index_by_depth))

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

  structure(
    result,
    original_index = index_by_depth,
    node_depths = node_depths,
    sep_pos = sep_pos,
    class = "nodes_in_depth"
  )
}

# reconstruct_paths ------------------------------------------------------------
reconstruct_paths <- function(x, depths = NULL)
{
  #x <- nodes_in_depth;depths = NULL
  stopifnot(inherits(x, "nodes_in_depth"))

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
