# cut.path_tree ----------------------------------------------------------------

#' Cut a Path Tree
#'
#' Reduce a path tree to its first levels.
#'
#' @param x tree object as returned by \code{kwb.fakin:::to_tree}
#' @param n_levels number of levels to which the tree is cut
#' @param depth current depth level
#' @param \dots further arguments (currently not used)
#'
#' @export
#'
cut.path_tree <- function(x, n_levels = 2, depth = 0, ...)
{
  if (depth == n_levels || ! is.list(x)) {

    ""

  } else {

    result <- lapply(x, function(subtree) {

      cut.path_tree(subtree, n_levels, depth + 1)
    })

    kwb.utils::addClass(result, className = "path_tree")
  }
}

# print_replacement_template ---------------------------------------------------
print_replacement_template <- function(x)
{
  kwb.utils::catLines(paste(x, x, sep = ";"))
}

# apply_substitutions_from_file ------------------------------------------------
apply_substitutions_from_file <- function(x, file, dbg = TRUE)
{
  y <- kwb.utils::multiSubstitute(x, read_substitutions_from_file(file))

  cat_changes_if(dbg, x, y)

  y
}

# read_substitutions_from_file -------------------------------------------------
read_substitutions_from_file <- function(file)
{
  substitution_data <- read_csv_de(file)

  kwb.utils::toLookupList(data = substitution_data)
}

# get_words_to_attribute_list --------------------------------------------------
get_words_to_attribute_list <- function(file)
{
  df <- read_csv_de(file)

  df <- df[df$attributes != "", ]

  df
}

# extract_properties -----------------------------------------------------------
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
extract_and_substitute <- function(pattern, replacement, x)
{
  result <- kwb.utils::extractSubstring(paste0("(", pattern, ")"), x, 1)

  is_matching <- result != ""

  result[is_matching] <- gsub(pattern, replacement, result[is_matching])

  result
}

# property_strings_to_data_frame -----------------------------------------------
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