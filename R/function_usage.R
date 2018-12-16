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
#'
#' @export
#'
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

  full_spec <- lengths(parts) > 1

  function_frequency$full_spec <- ifelse(full_spec, "full_spec", "no_full_spec")

  function_frequency$name[full_spec] <- sapply(parts[full_spec], "[", 2)

  function_frequency %>%
    tidyr::spread("full_spec", "count", fill = 0) %>%
    dplyr::mutate(total = full_spec + no_full_spec) %>%
    dplyr::arrange(dplyr::desc(total))
}

#' Which Function is Called How Often?
#'
#' @param tree parse tree as returned by \code{\link{parse_scripts}}
#' @return data frame with columns \code{name} (name of function), \code{count}
#'   (number of times the function is called)
get_function_call_frequency <- function(tree)
{
  raw_lines <- deparse(tree)

  function_calls <- sort(unlist(stringr::str_extract_all(
    raw_lines, "[A-Za-z][A-Za-z0-9.]*(::)?[A-Za-z][A-Za-z0-9._]*\\("
  )))

  function_calls <- gsub("\\($", "", function_calls)

  vector_to_count_table(function_calls)
}

# get_function_call_frequency_2 ------------------------------------------------
get_function_call_frequency_2 <- function(tree)
{
  result <- kwb.code:::extract_from_parse_tree(tree, dbg = TRUE)

  vector_to_count_table(unlist(result))
}
