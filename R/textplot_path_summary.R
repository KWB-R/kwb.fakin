# textplot_path_summary --------------------------------------------------------
textplot_path_summary <- function(data) {
  data %>%
    fakin.path.app:::get_path_summary() %>%
    text_summary_to_text() %>%
    kwb.plot::ggplot_text_lines()
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
