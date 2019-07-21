# textplot_path_summary --------------------------------------------------------
textplot_path_summary <- function(data) {
  data %>%
    get_path_summary() %>%
    text_summary_to_text() %>%
    kwb.plot::ggplot_text_lines()
}

# get_path_summary -------------------------------------------------------------
get_path_summary <- function(df, n = 3)
{
  is_pathlist <- inherits(df, "pathlist")

  paths <- if (is_pathlist) {
    as.character(df, relative = TRUE)
  } else {
    kwb.utils::pasteColumns(df, c("folder", "file"), "/")
  }

  file_types <- if (is_pathlist) {
    kwb.utils::selectColumns(df@data, "type")
  } # else NULL

  foldernames <- if (is_pathlist) {
    unique(pathlist::filename(df[file_types == "directory"]))
  } else {
    unique(unlist(kwb.file::split_paths(unique(df$folder))))
  }

  sizes <- kwb.utils::selectColumns(columns = "size", x = if (is_pathlist) {
    df@data
  } else {
    df
  })

  max_size_indices <- utils::head(order(sizes, decreasing = TRUE), n)

  path_lengths <- nchar(paths)

  filenames <- if (is_pathlist) {
    pathlist::filename(df[file_types == "file"])
  } else {
    df$file
  }

  file_lengths <- nchar(filenames)
  folder_lengths <- nchar(foldernames)

  files_longer_than <- function(n) filenames[sum(file_lengths > n)]
  folders_longer_than <- function(n) foldernames[sum(folder_lengths > n)]

  nchars_file <- stats::setNames(nm = c(100, 80, 60))
  nchars_folder <- stats::setNames(nm = c(50, 40, 30))

  df_maxsize <- if (is_pathlist) {
    df[max_size_indices]
  } # else NULL

  biggest_file <- if (is_pathlist) {
    as.character(df_maxsize, relative = TRUE)
  } else {
    paste0(df$folder[max_size_indices], "/", df$file[max_size_indices])
  }

  biggest_sizes <- if (is_pathlist) {
    kwb.utils::selectColumns(df[max_size_indices]@data, "size")
  } else {
    df$size[max_size_indices]
  }

  list(
    longest_path = utils::head(paths[which.max(path_lengths)], n),
    longest_file = utils::head(filenames[which.max(file_lengths)], n),
    longest_folder = utils::head(foldernames[which.max(folder_lengths)], n),
    biggest_file = structure(biggest_file, sizes = biggest_sizes),
    files_longer_than = lapply(nchars_file, files_longer_than),
    folders_longer_than = lapply(nchars_folder, folders_longer_than),
    percentage_good_filename = 100 * mean(name_is_ok(filenames))
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
