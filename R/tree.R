# flatten_tree -----------------------------------------------------------------
flatten_tree <- function(x)
{
  if (! is.list(x)) {
    return(x)
  }

  keys <- names(x)

  do.call(c, lapply(seq_along(x), function(i) {

    sub_paths <- flatten_tree(x[[i]])

    available <- sub_paths != ""

    paths <- rep(keys[i], length(sub_paths))

    paths[available] <- paste0(paths[available], "/", sub_paths[available])

    paths
  }))
}

# to_tree ----------------------------------------------------------------------

#' Convert Paths to Tree List
#'
#' @param x list as returned by \code{\link[base]{strsplit}}
#' @param dbg if \code{TRUE}, debug messages are shown
#'
#' @importFrom kwb.file split_paths
#' @importFrom kwb.utils addClass
#'
to_tree <- function(x, dbg = FALSE)
{
  if (! is.list(x)) {
    x <- kwb.file::split_paths(
      kwb.utils::removeDuplicates(as.character(x)), dbg = dbg
    )
  }

  # Get path depths
  depths <- sapply(x, length)

  # Keep only elements of non-zero depth
  selected <- depths > 0

  x <- x[selected]

  depths <- depths[selected]

  # Get first level folders/files
  first_elements <- sapply(x, "[", 1)

  # Get frequency of first level folders/files
  n <- table(first_elements)

  # Find the leafs of the tree
  leafs <- setdiff(first_elements[depths == 1], names(n)[n > 1])

  # Separate the leafs from the sub-trees
  is_tree <- ! first_elements %in% leafs

  # Build the sub-trees
  if (any(is_tree)) {

    trees <- lapply(split(x[is_tree], first_elements[is_tree]), lapply, "[", -1)

    trees <- lapply(trees, to_tree)

  } else {

    trees <- NULL
  }

  if (length(leafs) > 0) {

    trees <- c(structure(as.list(rep("", length(leafs))), names = leafs), trees)
  }

  kwb.utils::addClass(trees, "path_tree")
}

# to_tree_using_pathlist -------------------------------------------------------
to_tree_using_pathlist <- function(x, dbg = FALSE, depth. = 0)
{
  pl <- if (inherits(x, "pathlist")) {
    x
  } else {
    if (is.list(x)) {
      pathlist::pathlist(dbg = dbg, segments = x)
    } else {
      pathlist::pathlist(dbg = dbg, paths = kwb.utils::removeDuplicates(
        as.character(x)
      ))
    }
  }

  # There should not be any elements with a depth of zero
  stopifnot(all(pl@depths > 0))

  # Get first level folders/files
  first_elements <- pl@folders[, 1]

  # Get frequency of first level folders/files
  n <- table(first_elements)

  # Find the leafs of the tree
  leafs <- setdiff(unique(first_elements[pl@depths == 1]), names(n)[n > 1])

  # Separate the leafs from the sub-trees
  is_tree <- ! first_elements %in% leafs

  # Build the sub-trees
  trees <- if (any(is_tree)) {

    top_levels <- setdiff(unique(first_elements), leafs)

    trees <- lapply(stats::setNames(nm = top_levels), function(top_level) {
      top_level_paths <- methods::getMethod("$", "pathlist")(pl, top_level)
    })

    lapply(trees, to_tree_using_pathlist, depth. = depth. + 1)

  } # else NULL

  if (length(leafs)) {
    trees <- c(structure(as.list(rep("", length(leafs))), names = leafs), trees)
  }

  trees <- kwb.utils::addClass(trees, "path_tree")

  # If this is the top-level call, put the tree into a list with the root as the
  # top-level
  if (depth. == 0) {
    kwb.utils::addClass(stats::setNames(list(trees), pl@root), "path_tree")
  } else {
    trees
  }
}

# summary.path_tree ------------------------------------------------------------

#' Get Statistics on Path Tree Nodes
#'
#' @param object tree object as returned by \code{kwb.fakin:::to_tree}
#' @param \dots further arguments (currently not used)
#'
#' @export
#'
summary.path_tree <- function(object, ...)
{
  name_frequency <- table(get_names_recursively(object))

  result <- as.matrix(name_frequency[order(names(name_frequency))])

  dimnames(result) <- list(node_name = rownames(result), "occurrences")

  result
}

# get_names_recursively --------------------------------------------------------
get_names_recursively <- function(x)
{
  if (is.list(x)) {

    do.call(c, lapply(names(x), function(element) {

      c(element, get_names_recursively(x[[element]]))
    }))
  }
}

# cut.path_tree ----------------------------------------------------------------

#' Cut a Path Tree
#'
#' Reduce a path tree to its first levels.
#'
#' @param x tree object as returned by \code{kwb.fakin:::to_tree}
#' @param n_levels number of levels to which the tree is cut
#' @param depth current depth level
#' @param \dots further arguments (currently not used)
#'
#' @export
#'
cut.path_tree <- function(x, n_levels = 2, depth = 0, ...)
{
  if (depth == n_levels || ! is.list(x)) {

    ""

  } else {

    result <- lapply(x, function(subtree) {

      cut.path_tree(subtree, n_levels, depth + 1)
    })

    kwb.utils::addClass(result, className = "path_tree")
  }
}
