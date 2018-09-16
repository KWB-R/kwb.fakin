# get_names_of_used_packages ---------------------------------------------------

#' Get Names of Packages Used in R-Scripts
#'
#' @param root_dir directory in which to look recursively for R-scripts
#' @param pattern regular expression matching the names of the files to be
#'   considered
#'
#' @export
#'
get_names_of_used_packages <- function(root_dir, pattern = "[.][rR](md)?$")
{
  script_paths <- list.files(
    root_dir, pattern, full.names = TRUE, recursive = TRUE
  )

  package_usages <- lapply(script_paths, function(file) kwb.utils::catAndRun(
    paste("Analysing", file),
    grep("library", readLines(file), value = TRUE)
  ))

  usage_lines <- sort(unique(unlist(package_usages)))

  library_pattern <- "library\\(([^)]+)\\)"

  package_names <- kwb.utils::extractSubstring(library_pattern, usage_lines, 1)

  packages <- sort(unique(kwb.utils::multiSubstitute(package_names, list(
    "^\"|\"$" = "",
    "[\",].*$" = ""
  ))))

  packages[packages != ""]
}
