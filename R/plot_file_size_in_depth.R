# plot_file_size_in_depth ------------------------------------------------------
plot_file_size_in_depth <- function(
  df, max_depth = NULL, main = "", text_size = 3, point_size = 1,
  n_top_groups = 5, group_aesthetics = "shape", group_by = "extension"
)
{
  if (! is.data.frame(df) && is.list(df) && all(sapply(df, is.data.frame))) {

    projects <- stats::setNames(nm = names(df))
    #project <- projects[1]

    return(lapply(projects, function(project) {

      # Do not forget to add arguments if arguments are added to this function!
      plot_file_size_in_depth(
        df = df[[project]],
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

  plot_file_size_in_depth_gg(
    df = df,
    group_aesthetics = group_aesthetics,
    summary_data = summary_data,
    max_depth = max_depth,
    main = main,
    point_size = point_size,
    text_size = text_size
  )
}
