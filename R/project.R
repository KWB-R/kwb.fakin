# createLocalProject -----------------------------------------------------------

#' Create Empty Project Folder Structure Locally
#'
#' Copy the pure folder structure (without files) of a given KWB project from
#' the server to a new local folder below
#' "C:/Users/<user_name>/Documents/Projekte"
#'
#' @param project Project name. Must correspond with the name of a folder below
#'   one of the server locations returned by \code{\link{getProjectPaths}}
#' @param start_directory Path to the project network drive at KWB
#' @export
#'
createLocalProject <- function(
  project,
  start_directory = options()$kwb.fakin.paths$projects
)
{
  paths <- getProjectPaths(
    start_directory = start_directory,
    skip_pattern = NULL
  )

  index <- grep(paste0("/", project, "$"), paths)

  projects <- sapply(kwb.file::split_paths(paths), kwb.utils::lastElement)

  if (length(index) == 0L) {

    project_list <- kwb.utils::stringList(projects, collapse = "\n  ")

    cat("Available projects:\n ", project_list)

    stop("No such project: '", project, "'. See above for available projects.")
  }

  targetdir <- file.path(kwb.utils::get_homedir(), "Documents/Projekte")

  targetdir <- file.path(targetdir, projects[index])

  kwb.utils::createDirectory(targetdir)

  kwb.utils::copyDirectoryStructure(paths[index], targetdir)
}

# getProjectPaths --------------------------------------------------------------

#' Paths where to find project on the KWB server
#'
#' @param start_directory Path to the project network drive at KWB
#' @param as_list If \code{TRUE} (the default is \code{FALSE}) the paths
#'   are returned as a list with the folder names as list element names.
#' @param skip_pattern pattern matching paths to be removed from the returned
#'   path list. By default all paths containing underscore at the beginning of
#'   a subdirectory name are removed.
#' @return full paths to project folders as a vector of character or as a named
#'   list if \code{as_list = TRUE}.
#'
#' @export
#'
getProjectPaths <- function(
  start_directory, as_list = FALSE, skip_pattern = "/_"
)
{
  #start_directory <- "Y:"

  roots <- file.path(start_directory, c(
    "AUFTRAEGE/_Angebote_in_Arbeit",
    "AUFTRAEGE/_Auftraege_laufend",
    "SUW_Department/Projects",
    "GROUNDWATER/PROJECTS",
    "WWT_Department/PROJECTS"
  ))

  # Get the paths to all sub directories
  paths <- unlist(lapply(roots, list.dirs, recursive = FALSE))

  # Remove paths matching skip_pattern
  if (kwb.utils::defaultIfNULL(skip_pattern, "") != "") {
    paths <- paths[! grepl("/_", paths)]
  }

  # Convert to named list if requested
  if (as_list) {

    stats::setNames(as.list(paths), basename(paths))

  } else {

    paths
  }
}
