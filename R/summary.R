# summary.path_tree ------------------------------------------------------------

#' Get Statistics on Path Tree Nodes
#'
#' @param tree object of class \code{path_tree} as returned by
#'   \code{kwb.fakin:::to_tree}
#'
#' @export
#'
summary.path_tree <- function(tree)
{
  name_frequency <- table(get_names_recursively(tree))

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
