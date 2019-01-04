# install.packages("devtools")
# devtools::install_github("kwb-r/kwb.fakin")

# In Ubuntu, do not use pandoc that ships with RStudio but use pandoc that
# we install manually with:
#
# apt-get install pandoc
#

Sys.setenv(RSTUDIO_PANDOC = "/usr/bin/pandoc")

library("kwb.utils")

THIS_SCRIPT <- "extract_properties"

#file <- safePath("~/Desktop/tmp/folders_projects_2018-09-11.txt")
file <- safePath("~/Desktop/Data/FAKIN/folders_projects/folders_projects_2018-09-11.txt")
#file <- "~/Desktop/Data/FAKIN/paths_poseidon_projekte_2016_05_16.txt"

pkg_file <- function(name) system.file("extdata", name, package = "kwb.fakin")
file_composed <- pkg_file("replacements_composed-words.csv")
file_unify <- pkg_file("replacements_unify.csv")
#file_attribute_words <- pkg_file("words-to-attributes.csv")
file_attribute_words <- pkg_file("words-to-attributes_aquanes.csv")

#Sys.setlocale(locale = "de_DE.utf-8")
Sys.setlocale(locale = "german")

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # Try to restore a path tree or reread, recreate and store the tree
  if (fails(path_tree <- kwb.fakin:::restore("path_tree"))) {

    # Try to restore a vector of folder paths or recreate and store it
    if (fails(paths <- kwb.fakin:::restore("paths"))) {

      paths_raw <- kwb.fakin::read_paths(file, encoding = "UTF-8")
      paths <- kwb.file::remove_common_root(paths_raw)
      kwb.fakin:::store(paths, THIS_SCRIPT)
    }

    path_tree <- kwb.fakin:::to_tree(paths)
    kwb.fakin:::store(path_tree, THIS_SCRIPT)
  }

  # Select path trees by department
  department_trees <- list(
    SUW = path_tree$SUW_Department$Projects,
    WWT = path_tree$WWT_Department$Projects,
    GRW = path_tree$GROUNDWATER$PROJECTS
  )

  # Get folder frequencies down to a certain folder depth
  (folder_frequencies <- lapply(department_trees, get_folder_frequency, 5))

  (words <- rownames(folder_frequencies$GRW))
  (words <- sort(unique(unlist(strsplit(words, "\\s+")))))

  words[order(RecordLinkage::levenshteinSim("report", words))]

  grep("roh|raw", words, value = TRUE, ignore.case = TRUE)

  # Search the frequency tables for relevant patterns
  lapply(folder_frequencies, grep_frequency, "litera")

  print(department_trees$SUW, 2)

  # Get all unique folder names
  (folder_names <- rownames(folder_frequency))

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

# MAIN: Clear storage ----------------------------------------------------------
if (FALSE)
{
  kwb.fakin:::clear_storage("paths")
  kwb.fakin:::clear_storage("path_tree")
}

# fails ------------------------------------------------------------------------
fails <- function(expr)
{
  inherits(try(expr), "try-error")
}

# succeeds ---------------------------------------------------------------------
succeeds <- function(expr)
{
  ! fails(expr)
}

# get_folder_frequency ---------------------------------------------------------
get_folder_frequency <- function(tree, depth)
{
  summary(cut(tree, depth))
}

# grep_frequency ---------------------------------------------------------------
grep_frequency <- function(f, p)
{
  names <- rownames(f)
  f[grepl(p, names, ignore.case = TRUE), , drop = FALSE]
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

  subdir_list <- kwb.file:::split_paths(paths)

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
