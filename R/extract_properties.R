# extract_properties -----------------------------------------------------------

#' Extract Pattern-Defined Properties from Strings
#'
#' @param x vector of character
#' @param patterns vector of character
#' @param replacements vector of character
#' @param as_data_frame logical. If \code{TRUE} (the default is \code{FALSE}),
#'   a data frame is returned.
#' @export
#' @examples
#' # Define patterns to be matched against
#' patterns <- c(
#'   "[Bb]ericht",
#'   "[- ](\\d+)$",
#'   "Abschluss",
#'   "Zwischen",
#'   "_HS$"
#' )
#'
#' # Define property:value pairs (or even prop1:value1+prob2:value2+...)
#' # referring to parts of the pattern enclosed in parentheses with \1, \2, ...
#' replacements <- c(
#'   "type:report",
#'   "number:\\1",
#'   "stage:final",
#'   "stage:intermediate",
#'   "author:Sonnenberg+reviewed:true"
#' )
#'
#' # Define strings in which to look for properties and their values
#' x <- c("Bericht", "Bericht 1", "Abschlussbericht", "Zwischenbericht_HS")
#'
#' # Extract property values as strings
#' extract_properties(x = x, patterns, replacements)
#'
#' # Arrange the properties in a data frame
#' extract_properties(x = x, patterns, replacements, as_data_frame = TRUE)
extract_properties <- function(x, patterns, replacements, as_data_frame = FALSE)
{
  stopifnot(is.character(patterns), is.character(replacements))

  replacements <- kwb.utils::recycle(replacements, length(patterns))

  property_list <- lapply(seq_along(patterns), function(i) {

    extract_and_substitute(patterns[i], replacements[i], x)
  })

  property_strings <- sapply(seq_along(x), function(i) {

    kwb.utils::collapsed(remove_empty(sapply(property_list, "[", i)), "+")
  })

  if (as_data_frame) {

    property_strings_to_data_frame(property_strings, values = x)

  } else {

    stats::setNames(property_strings, x)
  }
}

# extract_and_substitute -------------------------------------------------------

#' Extract Substring Matching Pattern and Replace
#'
#' @param pattern pattern to be matched against the values in \code{x}. The
#'   pattern may contain pairs of parentheses surrounding parts of the pattern
#'   which can be referred to in the replacement string with \\1, \\2, etc., see
#'   example.
#' @param replacement string of the form \code{prop1:value1+prop2:value2+...}
#'   defining property names and values. In \code{value1}, \code{value2}, etc.
#'   it may be referred to parts of the matching string that correspond to
#'   the parts of the pattern encloded in parentheses, see example.
#' @param x vector of character in which to look for substrings to extract
#' @keywords internal
#' @examples
#' kwb.fakin:::extract_and_substitute(
#'   pattern = "key_(ab(c|d))",
#'   replacement = "alphabet:true+full:\\1+char2:\\2",
#'   x = c("noch viel mehr key_abc", "key_abd", "abe")
#' )
extract_and_substitute <- function(pattern, replacement, x)
{
  result <- kwb.utils::extractSubstring(paste0("(", pattern, ")"), x, 1)

  is_matching <- result != ""

  result[is_matching] <- gsub(pattern, replacement, result[is_matching])

  result
}

# property_strings_to_data_frame -----------------------------------------------

#' Arrange key1:value1+key2:value2 Strings in a Data Frame
#'
#' This function needs some improvement! The argument is called values but the
#' values will appear in column "name". Also, what happens if there is also a
#' property with name "name"?
#'
#' @param property_strings vector of character containing property values in the
#'   form of \code{prop1:value1+prop2:value2+prob3:value3+...}
#' @param values if given and not \code{NULL} this must be a vector of character
#'   as long as \code{property_strings}. The values will appear in a first
#'   column \code{name} of the returned data frame.
#' @keywords internal
#' @examples
#' property_strings <- c(
#'   "id:peter+class:person",
#'   "id:germany+class:country"
#' )
#'
#' kwb.fakin:::property_strings_to_data_frame(property_strings)
#' kwb.fakin:::property_strings_to_data_frame(property_strings, c("a", "b"))
#'
property_strings_to_data_frame <- function(property_strings, values = NULL)
{
  df <- kwb.utils::safeRowBindAll(lapply(property_strings, function(x) {

    key_value <- kwb.utils::toKeysAndValues(x, c("[+]", ":"))

    key_value$keys <- kwb.utils::makeUnique(key_value$keys, warn = FALSE)

    do.call(kwb.utils::toLookupTable, key_value)
  }))

  df <- df[, sort(names(df))]

  if (is.null(values)) {

    df

  } else {

    cbind(kwb.utils::noFactorDataFrame(name = values), df)
  }
}
