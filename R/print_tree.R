# print.path_tree --------------------------------------------------------------

#' Print a tree
#'
#' @param tree object of class \code{"path_tree"} as returned by
#'   \code{kwb.fakin:::to_tree}
#' @param max_depth number of depth levels to be printed
#' @param \dots further arguments (currently not used)
#'
#' @export
#'
print.path_tree <- function(tree, max_depth = 2, ...)
{
  if (is.list(tree)) {

    kwb.utils::catLines(tree_to_text(tree, max_depth))

  } else {

    cat("Tree is empty.\n")
  }
}

# tree_to_text -----------------------------------------------------------------
tree_to_text <- function(tree, max_depth = 1, depth = 1, indent = "")
{
  stopifnot(is.list(tree))

  chars <- c(
    space = " ", bottom_left = "\u2514", vertical = "\u2502",
    vertical_right = "\u251c"
  )

  n_children <- length(tree)

  child_names <- names(tree)

  do.call(c, lapply(seq_along(tree), function(i) {

    is_last <- (i == n_children)

    adapter_char <- chars[ifelse(is_last, "bottom_left", "vertical_right")]

    child_name <- child_names[i]

    textline <- paste0(indent, adapter_char, " ", child_name)

    child <- tree[[child_name]]

    child_lines <- if (depth < max_depth && ! identical(child, "")) {

      indent_char <- chars[ifelse(is_last, "space", "vertical")]

      new_indent <- paste0(indent, indent_char, " ")

      tree_to_text(child, max_depth, depth + 1, new_indent)
    }

    c(textline, child_lines)
  }))
}
