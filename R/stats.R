# get_file_duplicates ----------------------------------------------------------

#' Get list of paths containing files of the same name
#'
#' @param paths vector of character representing full file paths
#' @param pattern \code{paths} is matched against this pattern before the
#'   matching paths are split and analysed for duplicated files
#' @param ... arguments passed to \code{grep}, e.g. \code{ignore.case}
#'
#' @export
#'
#' @examples
#' paths <- c("/a/b/c.exe", "/a/b/d.exe", "/A/B/c.exe", "/A/B/d.exe")
#' get_file_duplicates(paths, pattern = "\\.exe$")
#'
get_file_duplicates <- function(paths, pattern, ...)
{
  paths_exe <- kwb.utils::rStylePath(grep(pattern, paths, value = TRUE, ...))

  files_exe <- sapply(kwb.file:::split_paths(paths_exe), kwb.utils::lastElement)

  duplicates_exe <- unique(files_exe[duplicated(files_exe)])

  lapply(kwb.utils::toNamedList(duplicates_exe), function(x) {

    paths_exe[files_exe == x]
  })
}

# ascii_stats ------------------------------------------------------------------
ascii_stats <- function(x)
{
  nonAscii <- ! isASCII(x)

  100 * prop.table(table(nonAscii))
}

# get_path_stat_matrix ---------------------------------------------------------

#' How Many Folders of the Template are in the Project Folder?
#'
#' @param project_folder folder in which to look for the projects of one
#'   KWB department, e.g. ".../SUW_Department/Projects"
#' @param template_folders vector of relative paths of folders expected to
#'   be contained within each folder within \code{project_folder}
#'
get_path_stat_matrix <- function(project_folder, template_folders)
{
  cat("Generating folder statistics for", project_folder, "...\n")

  paths <- dir(project_folder, full.names = TRUE, all.files = FALSE)

  paths <- remove_unwanted(paths)

  stats <- sapply(paths, function(path) {

    search_paths <- file.path(path, template_folders)

    n_found <- sapply(search_paths, function(x) sum(startsWith(paths, x)))

    names(n_found) <- gsub(
      check_or_set_ending_slash(path), "", names(n_found),
      fixed = TRUE
    )

    n_found
  })

  old_names <- if (length(dim(stats)) == 2) colnames(stats) else names(stats)

  x <- check_or_set_ending_slash(project_folder)

  new_names <- gsub(x, "", old_names, fixed = TRUE)

  if (length(dim(stats)) == 2) {

    colnames(stats) <- new_names

  } else {

    names(stats) <- new_names
  }

  stats
}

# remove_unwanted --------------------------------------------------------------
remove_unwanted <- function(x)
{
  x <- x[! grepl("(desktop\\.ini|Thumbs\\.db|\\.pdf|\\.lnk)$", x)]
  x <- x[! grepl("(cut and paste from_Proposals|projektspezifisch)$", x)]
  x <- x[! grepl("^_", basename(x))]

  x
}

# ncharTable -------------------------------------------------------------------
ncharTable <- function(x) table(nchar(x))

# ncharHist --------------------------------------------------------------------
ncharHist <- function(x) graphics::hist(nchar(x))
