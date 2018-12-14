if (FALSE)
{
  library(dplyr)

  tree <- kwb.code::parse_scripts(root = "C:/Users/hsonne/Documents/R-Development")

  function_call_frequency <- get_function_call_frequency(tree)

  View(function_call_frequency)

  package_function_usage <- get_package_function_usage(tree, package = "kwb.utils")

  View(package_function_usage)
}

# get_package_function_usage ---------------------------------------------------

#' How Often Are the Functions of a Package Used?
#'
#' @param tree parse tree as returned by \code{\link{parse_scripts}}
#' @param package name of the package (must be installed)
#'
#' @return data frame with columns \code{name} (name of the function),
#'   \code{prefixed} (number of function calls prefixed with \code{<package>::}
#'    or \code{<package>:::}), \code{non_prefixed} (number of function calls
#'    that are not prefixed with the package name) and \code{total} (total
#'    number of function calls)
#' @examples
#' # Read all scripts that are provided in the kwb.fakin package
#' tree <- kwb.code::parse_scripts(root = system.file(package = "kwb.fakin"))
#'
#' # Check which functions from kwb.utils are used and how often
#' get_package_function_usage(tree, "kwb.utils")
#'
#' # Hm, this does not seem to be the whole truth...
get_package_function_usage <- function(tree, package)
{
  `%>%` <- magrittr::`%>%`

  package_functions <- ls(getNamespace(package))

  patterns <- sprintf("^%s$", sort(c(
    package_functions,
    paste0(package, "::", package_functions),
    paste0(package, ":::", package_functions)
  )))

  x <- get_function_call_frequency(tree)

  function_frequency <- do.call(rbind, lapply(patterns, function(pattern) {

    x[grepl(pattern, x$name), ]
  }))

  parts <- strsplit(function_frequency$name, ":::?")

  uses_prefix <- lengths(parts) > 1

  function_frequency$uses_prefix <- uses_prefix

  function_frequency$Var1[uses_prefix] <- sapply(parts[uses_prefix], "[", 2)

  function_frequency %>%
    tidyr::spread(uses_prefix, count, fill = 0) %>%
    dplyr::mutate(total = `FALSE` + `TRUE`) %>%
    dplyr::arrange(desc(total)) %>%
    dplyr::select(
      name = "Var1", prefixed = `FALSE`, non_prefixed = `TRUE`, "total"
    )
}

#' Which Function is Called How Often?
#'
#' @param tree parse tree as returned by \code{\link{parse_scripts}}
#' @return data frame with columns \code{name} (name of function), \code{count}
#'   (number of times the function is called)
get_function_call_frequency <- function(root)
{
  raw_lines <- deparse(tree)

  function_calls <- sort(unlist(stringr::str_extract_all(
    raw_lines, "[A-Za-z][A-Za-z0-9.]*(::)?[A-Za-z][A-Za-z0-9._]*\\("
  )))

  function_frequency <- sort(table(function_calls), decreasing = TRUE)

  names(function_frequency) <- gsub("\\($", "", names(function_frequency))

  stats::setNames(
    as.data.frame(function_frequency, stringsAsFactors = FALSE),
    c("name", "count")
  )
}

