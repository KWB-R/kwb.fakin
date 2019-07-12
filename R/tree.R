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

    x <- kwb.file::split_paths(remove_duplicates(as.character(x)), dbg = dbg)
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
