# get_package_function_usage ---------------------------------------------------

#' How Often Are the Functions of a Package Used?
#'
#' @param tree parse tree as returned by \code{\link{parse_scripts}}
#' @param package name of the package (must be installed)
#' @inheritParams get_function_call_frequency
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
  package_functions <- ls(getNamespace(package), all.names = TRUE)

  functions <- sort(c(
    package_functions,
    paste0(package, "::", package_functions),
    paste0(package, ":::", package_functions)
  ))

  frequency_list <- get_function_call_frequency(tree, simple = simple)

  frequency_data <- dplyr::bind_rows(frequency_list, .id = "script")

  ff <- frequency_data[frequency_data$name %in% functions, ]

  ff <- digest_package_specifier(ff)

  if (by_script) {

    kwb.utils::resetRowNames(ff[order(ff$script, ff$name), ])

  } else {

    ff <- aggregate(. ~ name, kwb.utils::removeColumns(ff, "script"), sum)

    kwb.utils::resetRowNames(ff[order(- ff$count, ff$name), ])
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

  ff$name[is_explicit] <- sapply(parts[is_explicit], "[", 2)

  stats::aggregate(. ~ script + name, data = ff, FUN = sum)
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

      kwb.code:::extract_from_parse_tree(subtree)
    }

    vector_to_count_table(result) # may return NULL
  })
}
