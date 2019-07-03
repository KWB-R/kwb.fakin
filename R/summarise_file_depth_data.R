# summarise_file_depth_data ----------------------------------------------------
summarise_file_depth_data <- function(data_scatter)
{
  kwb.utils::checkForMissingColumns(data_scatter, c("root", "depth", "size"))

  summary_data <- data_scatter %>%
    dplyr::group_by(.data$root, .data$depth) %>%
    dplyr::summarise(n_files = dplyr::n(), total_size = sum(.data$size))

  lapply(
    X = split(summary_data, summary_data$root),
    FUN = add_percentage_of_sum_columns,
    columns = c("n_files", "total_size")
  )
}
