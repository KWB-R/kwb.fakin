# report_about_github_package --------------------------------------------------

#' Report about the Functions in an R-Package on GitHub
#'
#' @param repo path to the GitHub repository
#' @param \dots arguments passed to \code{\link{report_about_r_scripts}}
#' @export
#'
report_about_github_package <- function(repo, ...)
{
  report_about_r_scripts(
    root = github_package_path(repo, "R"),
    scripts = github_package_files(repo, "R"),
    ...
  )
}

# github_package_path ----------------------------------------------------------
github_package_path <- function(repo, path = "", branch = "master")
{
  sprintf("https://raw.githubusercontent.com/%s/%s/%s", repo, branch, path)
}

# github_package_files ---------------------------------------------------------
github_package_files <- function(repo, path)
{
  url <- sprintf("https://api.github.com/repos/%s/contents/%s", repo, path)

  json_code <- paste(readLines(url, warn = FALSE), collapse = "")

  jsonlite::fromJSON(json_code)$name
}
