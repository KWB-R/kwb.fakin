# prepare_for_scatter_plot -----------------------------------------------------
prepare_for_scatter_plot <- function(
  file_data, n_root_parts, start_path = NULL, min_depth = NULL
)
{
  stopifnot(is.data.frame(file_data))
  kwb.utils::checkForMissingColumns(file_data, c("path", "type", "size"))

  keep <- file_data$type == "file"

  if (! is.null(start_path)) {
    keep <- keep & startsWith(file_data$path, start_path)
  }

  file_data <- file_data[keep, , drop = FALSE]

  path_data <- kwb.file::split_into_root_folder_file_extension(
    file_data$path, n_root_parts
  )

  path_data$root <- as.character(kwb.file::remove_common_root(path_data$root))

  result <- cbind(path_data, size = file_data$size)

  if (is.null(min_depth)) {
    result
  } else {
    result[result$depth >= min_depth, , drop = FALSE]
  }
}
