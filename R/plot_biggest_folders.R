# plot_biggest_folders ---------------------------------------------------------

#' Plot Folders with Maximum Number of Files
#'
#' @param tree tree list structure as returned by \code{\link{to_tree}}
#' @param max_depth maximum number of folder depth
#' @param to_pdf if \code{TRUE} the output is directed to a temporary pdf file
#'
#' @export
#'
plot_biggest_folders <- function(tree, max_depth = 5, to_pdf = TRUE)
{
  pdf_file <- kwb.utils::preparePdfIf(to_pdf)

  kwb.plot::setMargins(left = 15)

  depth <- 0

  while (depth < max_depth) {

    tree_paths <- lapply(tree, flatten_tree)

    lengths <- sapply(tree_paths, length)

    graphics::barplot(lengths, horiz = TRUE, las = 1, cex.names = 0.8)

    tree <- tree[[which.max(lengths)]]

    depth <- depth + 1
  }

  kwb.utils::finishAndShowPdfIf(to_pdf, pdf_file)
}
