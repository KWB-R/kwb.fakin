# get_package_function_usage ---------------------------------------------------

#' How Often Are the Functions of a Package Used?
#'
#' @param tree parse tree as returned by \code{\link{parse_scripts}}
#' @param package name of the package (must be installed)
#' @inheritParams get_function_call_frequency
#' @param by_script if \code{TRUE} the functions are counted and returned by
#'   script, otherwise they are counted over all scripts
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
#' get_package_function_usage(tree, package = "kwb.utils")
#'
#' # Hm, this does not seem to be the whole truth...
get_package_function_usage <- function(
  tree, package, simple = FALSE, by_script = FALSE
)
{
  # Walk through the tree and collect all names of called functions
  frequency_list <- get_function_call_frequency(tree, simple = simple)
  frequency_data <- dplyr::bind_rows(frequency_list, .id = "script")

  packages <- remove_non_installed_packages(package)

  # For each package, filter for functions that are contained in the package
  result <- dplyr::bind_rows(.id = "package", lapply(
    X = stats::setNames(nm = packages),
    FUN = filter_for_package_functions,
    frequency_data = frequency_data
  ))

  if (nrow(result) == 0) {

    return(result)
  }

  if (by_script) {

    row_order <- order(result$package, result$script, result$name)

  } else {

    result <- stats::aggregate(
      . ~ package + name,
      kwb.utils::removeColumns(result, "script"),
      sum
    )

    row_order <- order(result$package, - result$count, result$name)
  }

  kwb.utils::resetRowNames(result[row_order, ])
}

# get_function_call_frequency --------------------------------------------------

#' Which Function is Called How Often?
#'
#' @param tree parse tree as returned by \code{\link{parse_scripts}}
#' @param simple if \code{TRUE}, a simple approach using a simple regular
#'   expression is used. This approach is fast but not correct as it e.g. counts
#'   function calls that are commented out or even string expressions that just
#'   look like function calls. Leaving this argument to its default,
#'   \code{FALSE}, will return only real function calls by evaluating the full
#'   structure of parse tree.
#' @param dbg if \code{TRUE}, debug messages are shown
#' @return data frame with columns \code{name} (name of function), \code{count}
#'   (number of times the function is called)
get_function_call_frequency <- function(tree, simple = FALSE, dbg = TRUE)
{
  stopifnot(is.list(tree))

  is_expression <- sapply(tree, is.expression)

  if (! all(is_expression)) {

    tree <- kwb.utils::catAndRun(
      messageText = sprintf(
        "Removing %d top level elements from the tree that are not expressions",
        sum(! is_expression)
      ),
      expr = tree[is_expression],
      dbg = dbg
    )

  }

  lapply(tree, function(subtree) {

    #subtree <- tree[[1]]
    result <- if (simple) {

      pattern <- "[A-Za-z][A-Za-z0-9.]*(::)?[A-Za-z][A-Za-z0-9._]*\\("

      function_names_list <- lapply(subtree, function(expr) {

        unlist(stringr::str_extract_all(deparse(expr), pattern))
      })

      gsub("\\($", "", unlist(function_names_list))

    } else {

      kwb.code:::extract_from_parse_tree(x = subtree)
    }

    vector_to_count_table(result) # may return NULL
  })
}

# remove_non_installed_packages ------------------------------------------------
remove_non_installed_packages <- function(packages)
{
  # Are packages as named in vector package installed?
  available <- packages %in% unname(utils::installed.packages()[, "Package"])

  if (all(available)) {

    return(packages)
  }

  message(
    "Skipping ", sum(! available), " package(s) that are not installed:\n",
    kwb.utils::stringList(packages[! available])
  )

  packages[available]
}

# filter_for_package_functions -------------------------------------------------
filter_for_package_functions <- function(frequency_data, package)
{
  package_functions <- ls(getNamespace(package), all.names = TRUE)

  functions <- sort(c(
    package_functions,
    paste0(package, "::", package_functions),
    paste0(package, ":::", package_functions)
  ))

  result <- frequency_data[frequency_data$name %in% functions, ]

  if (nrow(result) > 0) {

    digest_package_specifier(result)

  } else {

    result
  }
}

# digest_package_specifier -----------------------------------------------------
digest_package_specifier <- function(ff)
{
  kwb.utils::checkForMissingColumns(ff, c("script", "name", "count"))

  parts <- strsplit(ff$name, ":::?")

  is_explicit <- lengths(parts) > 1

  ff$explicit <- ifelse(is_explicit, ff$count, 0)

  ff$implicit <- ifelse(is_explicit, 0, ff$count)

  # Remove <package::[:]> if function is called with this package specifier
  if (any(is_explicit)) {

    ff$name[is_explicit] <- sapply(parts[is_explicit], "[", 2)
  }

  stats::aggregate(. ~ script + name, data = ff, FUN = sum)
}
