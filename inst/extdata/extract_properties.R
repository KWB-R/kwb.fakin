# install.packages("devtools")
# devtools::install_github("kwb-r/kwb.fakin")

library("kwb.utils")

file <- "~/Desktop/tmp/folders_projects_2018-09-11.txt"
#file <- "~/Desktop/Data/FAKIN/folders_projects/folders_projects_2018-09-11.txt"
#file <- "~/Desktop/Data/FAKIN/paths_poseidon_projekte_2016_05_16.txt"

pkg_file <- function(name) system.file("extdata", name, package = "kwb.fakin")
file_composed <- pkg_file("replacements_composed-words.csv")
file_unify <- pkg_file("replacements_unify.csv")
#file_attribute_words <- pkg_file("words-to-attributes.csv")
file_attribute_words <- pkg_file("words-to-attributes_aquanes.csv")

Sys.setlocale(locale = "de_DE.utf-8")

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # paths_raw <- kwb.fakin::read_paths(file, encoding = "UTF-8")
  # paths <- kwb.fakin::removeCommonRoot(paths_raw)

  # kwb.fakin:::clear_storage("paths")
  # kwb.fakin:::store(paths, "extract_properties")
  # paths <- kwb.fakin:::restore("paths")

  #folder_paths <- unique(dirname(paths))
  #folder_paths <- paths

  path_tree <- kwb.fakin:::to_tree(folder_paths)

  # kwb.fakin:::clear_storage("path_tree")
  # kwb.fakin:::store(path_tree, "extract_properties")
  # path_tree <- kwb.fakin:::restore("path_tree")

  kwb.fakin:::print_tree(path_tree)

  #subtree <- path_tree$SUW_Department$Projects
  subtree <- path_tree$WWT_Department$Projects

  first_levels <- kwb.fakin:::get_first_levels(tree = subtree, 2)

  # Get all unique folder names
  folder_names <- kwb.fakin:::sort_unique(unlist(first_levels))

  # Prepare file for replacements for composed words
  # kwb.fakin:::print_replacement_template(kwb.fakin:::sort_unique(folder_names))

  # Correct composed words

  folder_names_2 <- kwb.fakin:::apply_substitutions_from_file(
    folder_names, file_composed
  )

  folder_names_3 <- tolower(folder_names_2)

  # kwb.fakin:::print_replacement_template(kwb.fakin:::sort_unique(folder_names_2))

  property_defs <- kwb.fakin:::get_words_to_attribute_list(
    file = file_attribute_words
  )

  folder_properties <- kwb.fakin:::extract_properties(
    x = folder_names, # folder_names_3
    patterns = property_defs$word,
    replacements = property_defs$attributes,
    as_data_frame = FALSE
  )

  View(folder_properties)

  result <- get_path_properties(subtree$AquaNES, folder_properties)

  cat(yaml::as.yaml(result))

  frequencies <- sort(table(sapply(result, to_property_strings)))

  kwb.plot::setMargins(left = 14, top = 0, bottom = 2)

  barplot(tail(frequencies, 30), horiz = TRUE, cex.names = 0.6, las = 1)

  folder_names <- apply_substitutions_from_file(folder_names, file_unify)

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

# get_path_properties (rewritten, where is the original?) ----------------------
get_path_properties <- function(paths, folder_properties)
{
  if (is.list(paths)) {

    paths <- kwb.fakin:::flatten_tree(paths)
  }

  subdir_list <- kwb.fakin:::splitPaths(paths)

  stats::setNames(nm = paths, lapply(subdir_list, function(subdirs) {

    #subdirs <- subdir_list[[1]]
    properties <- folder_properties[subdirs]
    properties <- properties[! kwb.utils::isNaOrEmpty(properties)]
    properties <- kwb.fakin:::sort_unique(properties)
    properties <- kwb.utils::collapsed(properties, "+")
    key_values <- kwb.utils::toKeysAndValues(properties, c("\\+", ":"))
    key_values$keys <- kwb.utils::makeUnique(key_values$keys)
    do.call(kwb.utils::toLookupList, key_values)
  }))
}

# to_property_strings ----------------------------------------------------------
to_property_strings <- function(x)
{
  stopifnot(is.list(x))

  if (length(x)) {

    paste0(names(x), ":", x, collapse = "+" )

  } else {

    ""
  }
}
