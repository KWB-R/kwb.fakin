library(dplyr)
library(ggplot2)

# get_paths_to_latest_content_files --------------------------------------------
get_paths_to_latest_content_files <- function(month_string = "2018-12")
{
  content_files <- dir(full.names = TRUE, file.path(
    Sys.getenv("FAKIN_CONTENTS"), month_string
  ))

  last_date <- sort(unique(stringr::str_extract(content_files, "\\d{8}")))[1]

  content_files[grepl(last_date, content_files)]
}

# elements_per_depth -----------------------------------------------------------
elements_per_depth <- function(x, max_depth = NULL, depth = 1L)
{
  if (is.list(x)) {

    result <- data.frame(depth = depth, name = names(x))

    if (is.null(max_depth) || depth < max_depth) {

      result_below <- kwb.utils::rbindAll(lapply(
        x, elements_per_depth, max_depth, depth + 1L
      ))

      rbind(result, result_below)

    } else {

      result
    }
  }
}

latest_content_paths <- get_paths_to_latest_content_files("2018-12")

contents <- lapply(latest_content_paths, kwb.fakin:::read_file_info, sep = ",")

grobs <- lapply(contents, function(content) {

  file_tree <- kwb.fakin:::to_tree(kwb.fakin::removeCommonRoot(content$path))

  names_in_depth <- elements_per_depth(file_tree)

  element_counts <- names_in_depth %>%
    count(depth)

  element_counts %>%
    ggplot(aes(depth, n)) + geom_col() + scale_x_continuous(breaks = 1:10) +
    geom_text(aes(y = n + 500, label = n))
})
