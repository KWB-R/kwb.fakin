# createLocalProject -----------------------------------------------------------

#' Create Empty Project Folder Structure Locally
#'
#' Copy the pure folder structure (without files) of a given KWB project from
#' the server to a new local folder below
#' "C:/Users/<user_name>/Documents/Projekte"
#'
#' @param project Project name. Must correspond with the name of a folder below
#'   one of the server locations returned by \code{\link{getProjectPaths}}
#'
#' @export
#'
createLocalProject <- function(project)
{
  paths <- getProjectPaths(startDirectory = "//medusa/projekte$")

  index <- grep(paste0("/", project, "$"), paths)

  projects <- sapply(strsplit(paths, "/"), function(x) x[length(x)])

  if (length(index) == 0) {

    cat("Available projects:\n ", kwb.utils::stringList(projects, collapse = "\n  "))

    stop("No such project: '", project, "'. See above for available projects.")
  }

  paths[index]

  projects[index]

  targetdir <- file.path(
    kwb.utils::get_homedir(), "Documents/Projekte", projects[index]
  )

  kwb.utils::createDirectory(targetdir)

  kwb.utils::copyDirectoryStructure(paths[index], targetdir)
}

# getProjectPaths --------------------------------------------------------------

#' Paths where to find project on the KWB server
#'
#' @param startDirectory Path to the project network drive at KWB
#'
getProjectPaths <- function(startDirectory)
{
  folders <- c(
    "Auftraege",
    "SUW_Department/Projects",
    "GROUNDWATER/PROJECTS",
    "WWT_Department/PROJECTS"
  )

  paths <- file.path(startDirectory, folders)

  paths <- unlist(lapply(paths, dir, full.names = TRUE))

  paths[! grepl("/_|\\.(pdf|ini|db|lnk)$", paths)]
}
