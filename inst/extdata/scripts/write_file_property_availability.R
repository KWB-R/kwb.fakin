#
# Update info on property availablility in file "property_names.txt"
#

file <- kwb.fakin:::extdata_file("property_names.txt")

property_info <- read.table(
  file, sep = ",", header = TRUE, stringsAsFactors = FALSE
)

property_names <- property_info$property

# Filter for valid property names that can be used in a Search Index SQL query
available <- property_is_available(property_names)

property_info <- kwb.utils::noFactorDataFrame(
  property = property_names,
  available = available
)

property_file <- file.path(kwb.utils::desktop(), "tmp", "property_names.txt")

write.table(property_info, property_file, sep = ",", row.names = FALSE)

kwb.utils::hsOpenWindowsExplorer(dirname(property_file))
