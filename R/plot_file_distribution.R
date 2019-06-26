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
  kwb.utils::checkForMissingColumns(file_data, "path")

  data_scatter <- file_data %>%
    dplyr::filter(startsWith(.data$path, start_path)) %>%
    prepare_for_scatter_plot(n_root_parts = n_root_parts) %>%
    dplyr::filter(.data$depth > 0)
  # depth, root, size

  max_depth <- max(data_scatter$depth)

  data_by_root <- split(data_scatter, data_scatter$root)

  plots_scatter <- plot_file_size_in_depth(
    df = data_by_root,
    max_depth = max_depth,
    point_size = 1.5,
    ...
  )

  summary_data <- data_scatter %>%
    dplyr::group_by(.data$root, .data$depth) %>%
    dplyr::summarise(n_files = dplyr::n(), total_size = sum(.data$size))

  summaries <- split(summary_data, summary_data$root)

  summaries <- lapply(summaries, function(s) kwb.utils::setColumns(
    s,
    perc_n_files = kwb.utils::percentageOfSum(s$n_files),
    perc_total_size = kwb.utils::percentageOfSum(s$total_size)
  ))

  plots_perc <- lapply(summaries, plot_percentage_in_depth, max_depth)

  plots_text <- lapply(data_by_root, function(data) {
    kwb.plot::ggplot_text_lines(text_summary_to_text(get_path_summary(data)))
  })

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

# prepare_for_scatter_plot -----------------------------------------------------
prepare_for_scatter_plot <- function(file_data, n_root_parts)
{
  stopifnot(is.data.frame(file_data))

  kwb.utils::checkForMissingColumns(file_data, c("path", "type", "size"))

  file_data <- file_data[file_data$type == "file", ]

  path_data <- kwb.file::split_into_root_folder_file_extension(
    file_data$path, n_root_parts
  )

  path_data$root <- as.character(kwb.file::remove_common_root(path_data$root))

  cbind(path_data, size = file_data$size)
}

# plot_file_size_in_depth ------------------------------------------------------
plot_file_size_in_depth <- function(
  df, max_depth = NULL, main = "", text_size = 3, point_size = 1,
  n_top_groups = 5, group_aesthetics = "shape", group_by = "extension"
)
{
  if (! is.data.frame(df) && is.list(df) && all(sapply(df, is.data.frame))) {

    return(lapply(stats::setNames(nm = names(df)), function(project) {

      # Do not forget to add arguments if arguments are added to this function!
      plot_file_size_in_depth(
        df[[project]],
        max_depth = max_depth,
        main = project,
        text_size = text_size,
        point_size = point_size,
        n_top_groups = n_top_groups,
        group_aesthetics = group_aesthetics,
        group_by = group_by
      )
    }))
  }

  max_depth <- kwb.utils::defaultIfNULL(max_depth, max(df$depth))

  df$group <- if (group_by == "extension") {
    to_top_n(df$extension, n = n_top_groups)
  } else if (group_by == "level-1") {
    to_top_n(
      kwb.file::split_into_root_folder_file_extension(df$folder, 1)$root,
      n = n_top_groups
    )
  } else stop_(
    "group_by must be one of 'extension', 'level-1'"
  )

  summary_data <- df %>%
    dplyr::group_by(.data$root, .data$depth) %>%
    dplyr::summarise(n_files = dplyr::n(), total_size = sum(.data$size))

  df$size[df$size == 0] <- bytes_to_mib(0.1)

  y_label <- 100 * 1024

  label_captions <- data.frame(
    x = 0, y = y_label, label = "Size (MiB):\nFiles:"
  )

  ggplot2::ggplot(df, ggplot2::aes_string(x = "depth", y = "size")) +
    ggplot2::geom_point(
      do.call(
        what = ggplot2::aes_string,
        args = stats::setNames(list("group"), group_aesthetics)
      ),
      position = ggplot2::position_jitter(0.2), alpha = 0.7, size = point_size
    ) +
    ggplot2::geom_text(
      data = label_captions, size = text_size,
      ggplot2::aes_string(x = "x", y = "y", label = "label")
    ) +
    ggplot2::geom_text(
      data = summary_data, size = text_size,
      ggplot2::aes_string(
      x = "depth", y = "y_label",
      label = "sprintf('%0.1f\n%d', total_size, n_files)"
    )) +
    geom_hline_bytes() +
    scale_x_depth(max_depth) +
    scale_y_log_bytes() +
    ggplot2::xlab("") +
    ggplot2::ylab("File size") +
    ggplot2::ggtitle(sprintf(
      "%s: %0.0f MiB, %d files",
      main, sum(summary_data$total_size), sum(summary_data$n_files)
    )) +
    ggplot2::theme_minimal()
}

# to_top_n ---------------------------------------------------------------------
to_top_n <- function(x, n = 5, other = "<other>")
{
  x <- tolower(x)

  decreasingly_sorted_table <- function(xx) sort(table(xx), decreasing = TRUE)

  top_n <- names(decreasingly_sorted_table(x)[seq_len(n)])

  x[! x %in% top_n] <- other

  freqs <- decreasingly_sorted_table(x)

  labels <- sprintf("%s (%d)", names(freqs), as.integer(freqs))

  factor(x, levels = names(freqs), labels = labels)
}

# geom_hline_bytes -------------------------------------------------------------
geom_hline_bytes <- function()
{
  y <- bytes_to_mib(1024^(0:3))
  ggplot2::geom_hline(yintercept = y, colour = "darkgrey")
}

# scale_x_depth ----------------------------------------------------------------
scale_x_depth <- function(max_depth)
{
  ggplot2::scale_x_continuous(
    breaks = seq_len(max_depth), limits = 0.0 + c(0, max_depth)
  )
}

# scale_y_log_bytes ------------------------------------------------------------
scale_y_log_bytes <- function(x = c(1, 10, 100), small = FALSE)
{
  breaks <- x * rep(1024^(0:3), each = length(x))

  units <- c("Bytes", "KiB", "MiB", "GiB")

  break_labels <- unlist(lapply(units, function(unit) paste(x, unit)))

  if (small) {
    indices <- seq(1, by = 3, length.out = length(units))
    breaks <- breaks[indices]
    break_labels <- break_labels[indices]
  }

  ggplot2::scale_y_log10(
    breaks = bytes_to_mib(c(0.1, breaks)),
    labels = c("0 B", "1 Byte", break_labels[-1]),
    limits = bytes_to_mib(c(0.99 * 0.1, 100 * 1024^3))
  )
}

# plot_percentage_in_depth -----------------------------------------------------
plot_percentage_in_depth <- function(df, max_depth = max(df$depth))
{
  #df <- summaries[[3]]
  columns <- list("depth", "perc_n_files" = "files", "perc_total_size" = "size")

  df <- kwb.utils::hsMatrixToListForm(
    as.data.frame(kwb.utils::renameAndSelect(df, columns)),
    keyFields = "depth",
    colNamePar = "type",
    colNameVal = "percentage"
  )

  ggplot2::ggplot(df, ggplot2::aes_string(
    "depth", "percentage", linetype = "type"
  )) +
    ggplot2::geom_line() +
    scale_x_depth(max_depth) +
    ggplot2::xlab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
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

# boxplot_main_extensions ------------------------------------------------------
boxplot_main_extensions <- function(data, top_n = 10)
{
  extensions <- kwb.utils::selectColumns(data, "extension")

  data$extension <- to_top_n(extensions, n = top_n)

  data$size[kwb.utils::selectColumns(data, "size") == 0] <- 1

  graphics::boxplot(size ~ extension, data = data, log = "y", las = 2)
}

# get_path_summary -------------------------------------------------------------
get_path_summary <- function(data_scatter, n = 3)
{
  paths <- kwb.utils::pasteColumns(data_scatter, c("folder", "file"), "/")
  folders <- unique(unlist(kwb.file::split_paths(unique(data_scatter$folder))))
  sizes <- kwb.utils::selectColumns(data_scatter, "size")
  max_size_indices <- utils::head(order(sizes, decreasing = TRUE), n)

  path_lengths <- nchar(paths)
  file_lengths <- nchar(data_scatter$file)
  folder_lengths <- nchar(folders)

  files_longer_than <- function(n) data_scatter$file[sum(file_lengths > n)]
  folders_longer_than <- function(n) folders[sum(folder_lengths > n)]

  nchars_file <- stats::setNames(nm = c(100, 80, 60))
  nchars_folder <- stats::setNames(nm = c(50, 40, 30))

  list(
    longest_path = utils::head(paths[which.max(path_lengths)], n),
    longest_file = utils::head(data_scatter$file[which.max(file_lengths)], n),
    longest_folder = utils::head(folders[which.max(folder_lengths)], n),
    biggest_file = structure(
      paste0(
        data_scatter$folder[max_size_indices], "/",
        data_scatter$file[max_size_indices]
      ),
      sizes = data_scatter$size[max_size_indices]
    ),
    files_longer_than = lapply(nchars_file, files_longer_than),
    folders_longer_than = lapply(nchars_folder, folders_longer_than),
    percentage_good_filename = 100 * mean(
      name_is_ok(data_scatter$file)
    )
  )
}

# text_summary_to_text ---------------------------------------------------------
text_summary_to_text <- function(x)
{
  item_with_length <- function(xx) sprintf("- %s (%d)", xx, nchar(xx))

  c("Longest path(s):",
    item_with_length(x$longest_path),
    "Longest file(s):",
    item_with_length(x$longest_file),
    "Longest folder(s):",
    item_with_length(x$longest_folder),
    "Biggest file(s):",
    sprintf(
      "- %s (%0.1f MiB)",
      x$biggest_file, kwb.utils::getAttribute(x$biggest_file, "sizes")
    ),
    sprintf("Good file names: %0.1f %%", x$percentage_good_filename)
  )
}
