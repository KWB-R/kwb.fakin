# plot_file_size_in_depth_gg ---------------------------------------------------
plot_file_size_in_depth_gg <- function(
  df, group_aesthetics, summary_data, max_depth,
  main = "", point_size = 1, text_size = 3
)
{
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
      data = summary_data, size = text_size, ggplot2::aes_string(
        x = "depth",
        y = y_label,
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
