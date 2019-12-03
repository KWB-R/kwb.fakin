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

  df$group <- get_group_values(df, group_by, n_top_groups)

  summary_data <- df %>%
    dplyr::group_by(.data$root, .data$depth) %>%
    dplyr::summarise(n_files = dplyr::n(), total_size = sum(.data$size))

  df$size[df$size == 0] <- bytes_to_mib(0.1)

  fakin.path.app:::plot_file_size_in_depth_gg(
    df = df,
    group_aesthetics = group_aesthetics,
    summary_data = summary_data,
    max_depth = max_depth,
    main = main,
    point_size = point_size,
    text_size = text_size
  )
}

# get_group_values -------------------------------------------------------------
get_group_values <- function(df, group_by = "extension", n_top_groups = 4)
{
  values <- if (group_by == "extension") {

    kwb.utils::selectColumns(df, "extension")

  } else if (group_by == "level-1") {

    folders <- kwb.utils::selectColumns(df, "folder")

    kwb.file::split_into_root_folder_file_extension(folders, 1)$root

  } else stop_(

    "group_by must be one of 'extension', 'level-1'"
  )

  fakin.path.app:::to_top_n(values, n = n_top_groups)
}
