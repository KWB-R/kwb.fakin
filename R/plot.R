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

# plot_number_of_elements_per_folder -------------------------------------------

#' Plot Folders with Number of Direct Children
#'
#' @param x tree list structure as returned by \code{\link{to_tree}}
#' @param main title of the plot
#' @param to_pdf if \code{TRUE} the output is directed to a temporary pdf file
#' @param max_chars maximum number of characters to be used for file or folder
#'   names
#'
#' @export
#'
plot_number_of_elements_per_folder <- function(
  x, main = "", to_pdf = FALSE, max_chars = 20
)
{
  # Reduce to folders
  x <- x[sapply(x, is.list)]

  pdf_file <- kwb.utils::preparePdfIf(to_pdf)

  kwb.plot::setMargins(left = 8, right = 0)

  par_old <- graphics::par(mfrow = kwb.plot::bestRowColumnSetting(length(x)))

  on.exit(graphics::par(par_old))

  for (i in seq_along(x)) {

    folder <- kwb.utils::shorten(names(x)[i], max_chars)

    full_main <- if (main != "") paste(main, folder, sep = "\n") else folder

    graphics::barplot(
      sapply(x[[i]], length),
      names.arg = kwb.utils::shorten(names(x[[i]]), max_chars),
      horiz = TRUE, las = 1, cex.names = 0.7
    )

    graphics::title(full_main, cex.main = 1)
  }

  kwb.utils::finishAndShowPdfIf(to_pdf, pdf_file)
}
