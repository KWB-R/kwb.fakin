# extend_file_info -------------------------------------------------------------
extend_file_info <- function(file_info, n_keep = 1)
{
  paths <- kwb.file::remove_common_root(rownames(file_info), n_keep = n_keep)

  file_info <- kwb.utils::setColumns(
    file_info, pathString = paths,
    id = seq_len(nrow(file_info))
  )

  kwb.utils::resetRowNames(kwb.utils::moveColumnsToFront(file_info, "id"))
}
