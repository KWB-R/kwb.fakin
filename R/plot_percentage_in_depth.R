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
    fakin.path.app:::scale_x_depth(max_depth) +
    ggplot2::xlab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
}
