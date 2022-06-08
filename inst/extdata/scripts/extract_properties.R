# install.packages("remotes")
# remotes::install_github("kwb-r/kwb.fakin")

# In Ubuntu, do not use pandoc that ships with RStudio but use pandoc that
# we install manually with:
#
# apt-get install pandoc
#

Sys.setenv(RSTUDIO_PANDOC = "/usr/bin/pandoc")

library("kwb.utils")

#file <- safePath("~/Desktop/tmp/folders_projects_2018-09-11.txt")
file <- safePath("~/Desktop/Data/FAKIN/folders_projects/folders_projects_2018-09-11.txt")
#file <- "~/Desktop/Data/FAKIN/paths_poseidon_projekte_2016_05_16.txt"

pkg_file <- function(file) safePath(kwb.fakin::extdata_file(file))
file_composed <- pkg_file("config/replacements_composed-words.csv")
file_unify <- pkg_file("config/replacements_unify.csv")
file_attribute_words <- pkg_file("config/words-to-attributes.csv")
#file_attribute_words <- pkg_file("words-to-attributes_aquanes.csv")

#Sys.setlocale(locale = "de_DE.utf-8")
#Sys.setlocale(locale = "german")

# MAIN: Clear storage ----------------------------------------------------------
if (FALSE)
{
  kwb.fakin:::clear_storage("path_tree")
  kwb.fakin:::clear_storage("paths")
}

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # Read folder paths from a file and convert them to a tree structure. This
  # takes some time. Therefore, store() intermediate results and restore() if
  # stored objects are available. So, running the following line for the second
  # time is much faster than running it the first time.
  # Possible file encodings: "Windows-1252", "ISO-8859-1"
  path_tree <- provide_path_tree(file, fileEncoding = "ISO-8859-1")

  # What encoding is stored for the strings?
  table(Encoding(x <- kwb.fakin:::flatten_tree(path_tree)))

  # Select path trees by department
  department_trees <- list(
    SUW = path_tree$SUW_Department$Projects,
    WWT = path_tree$WWT_Department$Projects,
    GRW = path_tree$GROUNDWATER$PROJECTS
  )

  # Get folder name frequencies down to a certain folder depth
  (folder_frequencies <- lapply(department_trees, get_folder_frequency, 5))

  # Which different words are used in the Groundwater department?
  (words <- rownames(folder_frequencies$GRW))
  (words <- sort(unique(unlist(strsplit(words, "\\s+")))))

  # Order words by their similarity with the word "report" (most similar last)
  words[order(RecordLinkage::levenshteinSim("report", words))]

  # Look for folders that may contain raw data
  grep("roh|raw", words, value = TRUE, ignore.case = TRUE)

  # Search the frequency tables for relevant patterns
  lapply(folder_frequencies, grep_frequency, "litera")

  # Print a sub tree
  print(department_trees$SUW, 2)

  # Get all unique folder names used in the Surface Water department
  folder_frequency <- folder_frequencies$SUW
  (folder_names <- rownames(folder_frequency))

  # Correct composed words
  folder_names_2 <- kwb.fakin:::apply_substitutions_from_file(
    folder_names, file_composed
  )

  folder_names_3 <- tolower(folder_names_2)

  # Prepare file for replacements for composed words
  # kwb.fakin:::print_replacement_template(kwb.fakin:::sort_unique(folder_names))
  # kwb.fakin:::print_replacement_template(kwb.fakin:::sort_unique(folder_names_2))

  # Read assignments from word patterns to attributes
  property_defs <- kwb.fakin:::get_words_to_attribute_list(
    file = file_attribute_words
  )

  # Assign attributes to matching folder names
  folder_properties <- kwb.fakin::extract_properties(
    x = folder_names, # folder_names_3
    patterns = property_defs$word,
    replacements = property_defs$attributes,
    as_data_frame = FALSE
  )

  # Show the folder names together with the assigned attributes
  head(folder_properties[folder_properties != ""])

  # Select the Wastewater department subtree
  wwt_paths <- path_tree$WWT_Department$Projects

  # Try to extract properties from the AquaNES paths
  result <- get_path_properties(wwt_paths, folder_properties)

  # Filter for paths to which properties could be assigned
  result <- result[lengths(result) > 0]

  # Convert to YAML format and print
  cat(yaml::as.yaml(result))

  # Plot the frequencies of given properties
  plot_top_properties_distribution(result)

  # Unify folder names by applying substitutions that are stored in a file
  folder_names <- kwb.fakin:::apply_substitutions_from_file(
    folder_names, file_unify
  )

  # Show the ordered list of unified folder names
  kwb.fakin:::sort_unique(folder_names)

  # Split the folder names at underscore or space into words
  words <- kwb.fakin:::sort_unique(
    tolower(unlist(strsplit(folder_names, "[_ ]", folder_names)))
  )

  # Print the ordered list of words
  writeLines(words)

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

# provide_path_tree ------------------------------------------------------------
provide_path_tree <- function(file, fileEncoding)
{
  THIS_SCRIPT <- "extract_properties"

  # Try to restore a path tree or reread, recreate and store the tree
  if (kwb.fakin:::succeeds(path_tree <- kwb.fakin:::restore("path_tree"))) {
    return(path_tree)
  }

  # Try to restore a vector of folder paths or recreate and store it
  if (kwb.fakin:::fails(paths <- kwb.fakin:::restore("paths"))) {

    metadata <- fakin.path.app:::guess_file_metadata(file)
    metadata$fileEncoding <- fileEncoding
    paths <- fakin.path.app::read_file_paths(file, metadata)$path
    paths <- kwb.file::remove_common_root(paths)

    kwb.fakin:::store(paths, THIS_SCRIPT)
  }

  path_tree <- kwb.fakin:::to_tree(paths)

  kwb.fakin:::store(path_tree, THIS_SCRIPT)

  path_tree
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

# get_path_properties (rewritten, where is the original?) ----------------------
get_path_properties <- function(paths, folder_properties)
{
  if (is.list(paths)) {

    paths <- kwb.fakin:::flatten_tree(paths)
  }

  subdir_list <- kwb.file::split_paths(paths)

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

# plot_top_properties_distribution ---------------------------------------------
plot_top_properties_distribution <- function(result)
{
  # Restore current graphic parameters on exit
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Generate property strings and show the most frequent properties
  frequencies <- sort(table(sapply(result, to_property_strings)))

  # Set margins for a barplot of the frequencies
  kwb.plot::setMargins(left = 14, top = 0, bottom = 2)

  # Plot the frequencies
  barplot(tail(frequencies, 30), horiz = TRUE, cex.names = 0.6, las = 1)
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
