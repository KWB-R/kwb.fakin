library("kwb.utils")

if (FALSE)
{
  kwb.fakin::report_about_github_package(repo = "KWB-R/kwb.fakin")
  kwb.fakin::report_about_r_scripts("~/RProgramming/github/kwb.code")
  report_about_github_package(repo = "tidyverse/dplyr")
}

pattern_source <- "[.](r|rmd)$"
pattern_path <- "([\"'][A-Z]:[/\\][^\"']+\")"
pattern_path <- "([\"'](//|\\\\)[^/\\]+[/\\][^\"']+[\"'])"
#cat(pattern_path)

start_dir <- safePath(desktop(), "R-Development")

paths <- dir(
  start_dir, pattern = pattern_source, ignore.case = TRUE, recursive = TRUE,
  full.names = TRUE
)

path_tree <- kwb.fakin:::to_tree(paths)

fakin.path.app::plot_path_network(
  path_tree[[1]]$home$hauke$Desktop$`R-Development`$RScripts,
  3, width = 8000, fontSize = 20
)

grep_results <- lapply(paths, function(file) {
  content <- readLines(file, warn = FALSE)
  stringi::stri_trim_both(grep(pattern_path, content, value = TRUE))
})

looks_like_path <- sapply(grep_results, length) > 0
path_usages <- sort(unique(unlist(grep_results[looks_like_path])))

length(path_usages)
head(path_usages)
catLines(path_usages)

path_constants <- kwb.utils::extractSubstring(pattern_path, path_usages, 1)
unique_directories <- sort(unique(dirname(path_constants)))

length(unique_directories)
catLines(unique_directories)
