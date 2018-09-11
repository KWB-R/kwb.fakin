# get_first_levels -------------------------------------------------------------
get_first_levels <- function(tree, n_levels)
{
  tree <- tree[sapply(tree, is.list)]

  lapply(tree, function(subtree) {
    subpaths <- toSubdirMatrix(flatten_tree(subtree))
    apply(subpaths[, seq_len(min(ncol(subpaths), n_levels))], 2, function(x) {
      sort(unique(x[x != ""]))
    })
  })
}

# sort_unique ------------------------------------------------------------------
sort_unique <- function(x)
{
  sort(unique(x))
}

# print_replacement_template ---------------------------------------------------
print_replacement_template <- function(x)
{
  kwb.utils::catLines(paste(x, x, sep = ";"))
}

# correct_composed_words -------------------------------------------------------
correct_composed_words <- function(x, file, dbg = TRUE)
{
  data <- utils::read.csv2(file)

  x_new <- kwb.utils::multiSubstitute(x, kwb.utils::toLookupList(data = data))

  catChangesIf(dbg, x, x_new)

  x_new
}

# catChangesIf -----------------------------------------------------------------
catChangesIf <- function(dbg, x, y)
{
  if (dbg) {

    catChanges(x, y)
  }
}

# catChanges -------------------------------------------------------------------
catChanges <- function(x, y)
{
  is_modified <- x != y

  kwb.utils::catLines(sprintf("%s -> %s", x[is_modified], y[is_modified]))
}

# get_words_to_attribute_list --------------------------------------------------
get_words_to_attribute_list <- function(file)
{
  df <- utils::read.csv2(file, stringsAsFactors = FALSE)

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

  remove_empty <- function(x) x[x != ""]

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

  if (! is.null(values)) {

    cbind(kwb.utils::noFactorDataFrame(name = values), df)

  } else {

    df
  }
}
