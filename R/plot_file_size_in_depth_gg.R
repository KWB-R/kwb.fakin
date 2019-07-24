# plot_file_size_in_depth_gg ---------------------------------------------------
plot_file_size_in_depth_gg <- function(
  df, group_aesthetics = c("colour", "shape")[1], summary_data = NULL,
  max_depth = 0L, min_depth = 0L, main = "Total", point_size = 1,
  text_size = 3
)
{
  stopifnot(group_aesthetics %in% c("shape", "colour"))

  gg <- ggplot2::ggplot(df, get_depth_size_aesthetics("label" %in% names(df))) +
    ggplot2::geom_point(
      get_group_aesthetics(group_aesthetics),
      position = ggplot2::position_jitter(0.2), alpha = 0.7, size = point_size
    )

  gg <- add_size_count_labels(gg, summary_data, text_size = text_size)

  max_depth <- kwb.utils::defaultIfZero(
    max_depth, max(kwb.utils::selectColumns(df, "depth"))
  )

  min_depth <- kwb.utils::defaultIfZero(
    min_depth, min(kwb.utils::selectColumns(df, "depth"))
  )

  gg +
    geom_hline_bytes() +
    scale_x_depth(max_depth, min_depth) +
    scale_y_log_bytes() +
    ggplot2::xlab("") +
    ggplot2::ylab("File size") +
    ggplot2::ggtitle(get_title_with_size_count_info(main, summary_data)) +
    ggplot2::theme_minimal()
}

# get_depth_size_aesthetics ----------------------------------------------------
get_depth_size_aesthetics <- function(add_labels = FALSE)
{
  aes_args <- list(x = "depth", y = "size")

  if (add_labels) {
    aes_args <- c(aes_args, text = "label")
  }

  do.call(ggplot2::aes_string, aes_args)
}

# get_group_aesthetics ---------------------------------------------------------
get_group_aesthetics <- function(group_aesthetics = "colour")
{
  do.call(
    what = ggplot2::aes_string,
    args = stats::setNames(list("group"), group_aesthetics)
  )
}

# add_size_count_labels --------------------------------------------------------
add_size_count_labels <- function(
  gg, summary_data = NULL, y_label = 100 * 1024, text_size = 3
)
{
  stopifnot(identical(class(gg), c("gg", "ggplot")))

  if (is.null(summary_data)) {
    return(gg)
  }

  kwb.utils::checkForMissingColumns(summary_data, "depth")

  gg + ggplot2::geom_text(
    mapping = ggplot2::aes_string(x = "x", y = "y", label = "label"),
    data = data.frame(x = 0, y = y_label, label = "Size (MiB):\nFiles:"),
    size = text_size
  ) + ggplot2::geom_text(
    data = summary_data, size = text_size, ggplot2::aes_string(
      x = "depth", y = y_label,
      label = "sprintf('%0.1f\n%d', total_size, n_files)"
    )
  )
}

# geom_hline_bytes -------------------------------------------------------------
geom_hline_bytes <- function()
{
  y <- bytes_to_mib(1024^(0:3))
  ggplot2::geom_hline(yintercept = y, colour = "darkgrey")
}

# scale_x_depth ----------------------------------------------------------------
scale_x_depth <- function(max_depth, min_depth = 0)
{
  ggplot2::scale_x_continuous(
    breaks = seq_len(max_depth), limits = 0.0 + c(min_depth, max_depth)
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

# get_title_with_size_count_info -----------------------------------------------
get_title_with_size_count_info <- function(main = "", summary_data = NULL)
{
  if (is.null(summary_data)) {
    return(main)
  }

  sprintf(
    "%s: %0.0f MiB, %d files",
    main,
    sum(kwb.utils::selectElements(summary_data, "total_size")),
    sum(kwb.utils::selectElements(summary_data, "n_files"))
  )
}
