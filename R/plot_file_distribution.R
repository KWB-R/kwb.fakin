# plot_file_distribution -------------------------------------------------------

#' Plot Distributions of Files in Folder Depths and Name Summaries
#'
#' @param file_data data frame with columns \code{path}, \code{type},
#'   \code{size}
#' @param start_path only paths from \code{file_data} are considered that start
#'   with this path
#' @param n_root_parts number of first path segments to be considered as "root"
#' @param \dots further arguments passed to
#'   \code{kwb.fakin:::plot_file_size_in_depth}
#' @param to_pdf if \code{TRUE} (default) the results are plotted into a
#'   temporary pdf file that is opened in a pdf viewer
#' @export
#' @examples
#' # Set a seed for the random number generator
#' set.seed(20190625)
#'
#' # Create random paths
#' paths <- kwb.pathdict::random_paths()
#'
#' # Number of paths
#' n <- length(paths)
#'
#' # Create artificial file data (invent sizes)
#' file_data <- kwb.utils::noFactorDataFrame(
#'   path = paths,
#'   type = "file",
#'   size = abs(rnorm(n)) * kwb.fakin:::bytes_to_mib(
#'     2^sample(30, n, replace = TRUE)
#'   )
#' )
#'
#' kwb.fakin::plot_file_distribution(
#'   file_data, start_path = "reason", n_root_parts = 1, to_pdf = FALSE
#' )
#'
plot_file_distribution <- function(
  file_data, start_path, n_root_parts, ..., to_pdf = TRUE
)
{
  #kwb.utils::assignPackageObjects("kwb.fakin")
  data_scatter <- prepare_for_scatter_plot(
    file_data, n_root_parts, start_path, min_depth = 1
  )

  max_depth <- max(data_scatter$depth)

  data_by_root <- split(data_scatter, data_scatter$root)

  plots_scatter <- plot_file_size_in_depth(
    df = data_by_root, max_depth, point_size = 1.5, ...
  )

  summaries <- summarise_file_depth_data(data_scatter)

  plots_perc <- lapply(summaries, plot_percentage_in_depth, max_depth)

  plots_text <- lapply(data_by_root, textplot_path_summary)

  # Group plots belonging to the same project and arrange plots in a PDF file
  pdf_file <- kwb.utils::preparePdfIf(to_pdf)
  print(arrange_file_in_depth_plots(plots_scatter, plots_perc, plots_text))
  kwb.utils::finishAndShowPdfIf(to_pdf, pdf_file)

  invisible(list(
    data_scatter = data_scatter,
    plots_scatter = plots_scatter,
    plots_perc = plots_perc
  ))
}

# arrange_file_in_depth_plots --------------------------------------------------
arrange_file_in_depth_plots <- function(plots_scatter, plots_perc, plots_text)
{
  old_theme <- ggplot2::theme_get()
  ggplot2::theme_set(ggplot2::theme_minimal())
  on.exit(ggplot2::theme_set(old_theme))

  plots <- lapply(names(plots_scatter), function(project) {
    #project <- names(plots_scatter)[1]
    message(project)
    top_plots <- cowplot::plot_grid(
      plots_scatter[[project]], plots_perc[[project]],
      rel_heights = c(0.8, 0.2), ncol = 1, align = "v", axis = "lr"
    )
    cowplot::plot_grid(
      top_plots, plots_text[[project]], ncol = 1, rel_heights = c(0.6, 0.4)
    )
  })

  plots
}
