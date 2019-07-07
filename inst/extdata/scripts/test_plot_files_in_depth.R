# MAIN 3 -----------------------------------------------------------------------
if (FALSE)
{
  path_info <- kwb.fakin::read_path_information(
    "~/Desktop/Data/FAKIN/file-info_by-department/", sep = ";"
  )

  file_data <- path_info$`d2018-09-17_GROUNDWATER`

  data_scatter <- kwb.fakin:::prepare_for_scatter_plot(
    file_data, n_root_parts = 1, start_path = "", min_depth = 1
  )

  # folder, file, extension, depth, size
  head(data_scatter)

  max_depth <- max(data_scatter$depth)

  df <- split(data_scatter, data_scatter$root)

  plots_scatter <- kwb.fakin:::plot_file_size_in_depth(
    df = df, max_depth, point_size = 1.5
  )
}
