# install.packages("devtools")
# devtools::install_github("kwb-r/kwb.fakin")

library("kwb.utils")

#file <- "Desktop/Data/FAKIN/paths_poseidon_projekte.txt"
#file_composed <- "~/KWB/replacements_composed-words.csv"
#file_attribute_words <- "~/KWB/words-to-attributes.csv"

pkg_file <- function(name) system.file("extdata", name, package = "kwb.fakin")
file <- "~/Desktop/tmp/paths_projekte_2.txt"
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

  first_levels <- get_first_levels(tree = path_tree$SUW_Department$Projects, 2)

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
    as_data_frame = TRUE
  )

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
