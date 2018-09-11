# install.packages("devtools")
# devtools::install_github("kwb-r/kwb.fakin")

library("kwb.utils")

file <- "Desktop/Data/FAKIN/paths_poseidon_projekte.txt"
#file <- "~/Desktop/tmp/paths_projekte_2.txt"

pkg_file <- function(name) system.file("extdata", name, package = "kwb.fakin")
file_composed <- pkg_file("replacements_composed-words.csv")
file_attribute_words <- pkg_file("words-to-attributes.csv")

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # paths_raw <- kwb.fakin::read_paths(file)
  # paths <- kwb.fakin::removeCommonRoot(paths_raw)

  # kwb.fakin:::store(paths, "extract_properties")
  # paths <- kwb.fakin:::restore("paths")

  folder_paths <- unique(dirname(paths))

  path_tree <- kwb.fakin:::to_tree(folder_paths)

  # kwb.fakin:::store(path_tree, "extract_properties")
  # path_tree <- kwb.fakin:::restore("path_tree")

  subtree <- path_tree$SUW_Department$Projects

  first_levels <- get_first_levels(tree = subtree, 2)

  # Get all unique folder names
  folder_names <- sort_unique(unlist(first_levels))

  # Prepare file for replacements for composed words
  # print_replacement_template(sort_unique(folder_names))

  # Correct composed words
  folder_names_2 <- correct_composed_words(folder_names, file_composed)

  folder_names_3 <- tolower(folder_names_2)
  catChanges(folder_names_2, folder_names_3)

  # print_replacement_template(sort_unique(folder_names_2))

  property_defs <- get_words_to_attribute_list(file = file_attribute_words)

  folder_properties <- extract_properties(
    x = folder_names_3,
    patterns = property_defs$word,
    replacements = property_defs$attributes,
    as_data_frame = FALSE
  )

  result <- get_path_properties(subtree, folder_properties)

  kwb.plot::setMargins(left = 14, top = 0, bottom = 2)
  barplot(tail(sort(table(result)), 30), horiz = TRUE, cex.names = 0.6, las = 1)

  file <- tempfile("subdir_list_", fileext = ".yml")
  yaml::write_yaml(subdir_list, file = )

  dim(folder_properties)
  View(folder_properties)

  substitutions <- list(
    "\\s+|-+" = "_",
    "_+" = "_",
    "^_" = "",
    "(\\d+)(st|nd|rd|th)(.*)" = "\\2_no-\\1",
    "(\\d+)[.]_?zwischenbericht" = "zwischenbericht_\\1",
    "^\\d+_" = "",
    "^(wp|ap|tp)(_?(\\d|$))" = "wp_\\2",
    "^ppt$" = "powerpoint"
  )

  folder_names <- kwb.utils::multiSubstitute(folder_names, substitutions)

  sort_unique(folder_names)

  words <- sort_unique(tolower(unlist(strsplit(folder_names, "[_ ]", folder_names))))

  kwb.utils::catLines(words)

  # "(\\d+)(st|nd|rd|th)" = c(volume = 1),
  # "(Bogota|Sofia|Montbeliard|Braunschweig)" = c(city = 1),
  # "(\\d+)[.]\\s*(Zwischenbericht)" = c(report_volume = 1, report_type = 2),
  # "(BMBF)_(Zwischenbericht)_(\\d+)" = c(
  #   report_volume = 3, report_type = 2, funder = 1
  # ),
  # "(Zwischenbericht)e?" = c(report_type = 1),
  # "(Power[pP]oint)" = c(file_type = 1),
  # "(AP|WP|TP)\\s*(\\d+)([_ ](.*))?$" = c(wp_number = 2, wp_name = 4),
  # "(Clara|Malte|Nathalie|Julia|Kabbe|Malte|Wolfgang)" = c(editor = 1),
  # "(draft)" = c(version = 1),
  # "(([pP]re)?([pP]roposal))" = c(project_phase = 1)

  kwb.utils::catLines(attributes[grep("@", attributes)])

  attributes[grep("@", attributes, invert = TRUE)]
}

# TEST AREA --------------------------------------------------------------------
if (FALSE)
{
  x <- c("noch viel mehr key_abc", "key_abd", "abe")

  extract_and_substitute("key_(ab(c|d))", "alphabet:true+full:\\1+char2:\\2", x)
}

# get_first_levels -------------------------------------------------------------
get_first_levels <- function(tree, n_levels)
{
  tree <- tree[sapply(tree, is.list)]

  lapply(tree, function(subtree) {
    subpaths <- kwb.fakin:::toSubdirMatrix(kwb.fakin:::flatten_tree(subtree))
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
  data <- read.csv2(file)

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

  catLines(sprintf("%s -> %s", x[is_modified], y[is_modified]))
}

# get_words_to_attribute_list --------------------------------------------------
get_words_to_attribute_list <- function(file)
{
  df <- read.csv2(file, stringsAsFactors = FALSE)

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

# remove_empty -----------------------------------------------------------------
remove_empty <- function(x)
{
  x[x != ""]
}

# extract_and_substitute -------------------------------------------------------
extract_and_substitute <- function(pattern, replacement, x)
{
  result <- extractSubstring(paste0("(", pattern, ")"), x, 1)

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

# get_path_properties ----------------------------------------------------------
get_path_properties <- function(subtree, folder_properties)
{
  non_empty_properties <- remove_empty(folder_properties)

  paths_lower <- tolower(kwb.fakin:::flatten_tree(subtree))

  subdir_list <- kwb.fakin:::splitPaths(paths_lower)

  sapply(subdir_list, function(folders) {

    #folders <- subdir_list[[1]]

    property_strings <- non_empty_properties[folders[-1]]

    non_empty_properties <- unname(property_strings[! is.na(property_strings)])

    paste(collapse = "+", non_empty_properties)
  })
}
