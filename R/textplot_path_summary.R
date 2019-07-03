# textplot_path_summary --------------------------------------------------------
textplot_path_summary <- function(data) {
  data %>%
    get_path_summary() %>%
    text_summary_to_text() %>%
    kwb.plot::ggplot_text_lines()
}

# get_path_summary -------------------------------------------------------------
get_path_summary <- function(data_scatter, n = 3)
{
  paths <- kwb.utils::pasteColumns(data_scatter, c("folder", "file"), "/")
  folders <- unique(unlist(kwb.file::split_paths(unique(data_scatter$folder))))
  sizes <- kwb.utils::selectColumns(data_scatter, "size")
  max_size_indices <- utils::head(order(sizes, decreasing = TRUE), n)

  path_lengths <- nchar(paths)
  file_lengths <- nchar(data_scatter$file)
  folder_lengths <- nchar(folders)

  files_longer_than <- function(n) data_scatter$file[sum(file_lengths > n)]
  folders_longer_than <- function(n) folders[sum(folder_lengths > n)]

  nchars_file <- stats::setNames(nm = c(100, 80, 60))
  nchars_folder <- stats::setNames(nm = c(50, 40, 30))

  list(
    longest_path = utils::head(paths[which.max(path_lengths)], n),
    longest_file = utils::head(data_scatter$file[which.max(file_lengths)], n),
    longest_folder = utils::head(folders[which.max(folder_lengths)], n),
    biggest_file = structure(
      paste0(
        data_scatter$folder[max_size_indices], "/",
        data_scatter$file[max_size_indices]
      ),
      sizes = data_scatter$size[max_size_indices]
    ),
    files_longer_than = lapply(nchars_file, files_longer_than),
    folders_longer_than = lapply(nchars_folder, folders_longer_than),
    percentage_good_filename = 100 * mean(
      name_is_ok(data_scatter$file)
    )
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
