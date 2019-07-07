# Load magritter to be able to use the pipe operator %>%
library(magrittr)

# Provide paths to file path files in different formats in a list
files <- as.list(c(
  kwb.file::dir_full("~/Desktop/Data/FAKIN/example_pathFiles"),
  "~/Downloads/files_desktop_powershell.csv",
  kwb.file::dir_full("~/Desktop/Data/FAKIN/file-info_by-department", "^path")
))

# Name the list elements
names(files) <- kwb.utils::removeExtension(basename(unlist(files)))

# Show the list of files
str(files)

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  metadata_list <- kwb.fakin:::extend_each_element(
    lapply(files, kwb.fakin:::guess_file_metadata, n_first_rows = 10),
    encoding = "ISO-8859-1",
    encoding_fread = "Latin-1"
  )

  print_metadata_list(metadata_list)
  do.call(rbind, metadata_list)

  contents <- lapply(seq_along(files), function(i) {
    kwb.fakin::read_file_paths(file = files[[i]], metadata = metadata_list[[i]])
  })

  contents <- kwb.utils::excludeNULL(contents)

  lapply(contents, function(x) table(kwb.utils::fileExtension(x[[1]])))
}

# MAIN 2 -----------------------------------------------------------------------
if (FALSE)
{
  file <- files$files_desktop_powershell

  path_data <- kwb.fakin::read_file_paths(file)

  table(path_data$type)

  #kwb.fakin::plot_treemaps_from_path_data(path_data) # -> error
}

# MAIN 3 -----------------------------------------------------------------------
if (FALSE)
{
  path_info <- kwb.fakin::read_path_information(
    "~/Desktop/Data/FAKIN/file-info_by-department"
  )

  path_info_2 <- lapply(
    files[grepl("^path-info_", names(files))],
    kwb.fakin::read_file_paths
  )

  # Check identity of the results
  stopifnot(all(sapply(seq_along(path_info), function(i) {
    identical(path_info[[i]], path_info_2[[i]])
  })))
}

# print_metadata_list ----------------------------------------------------------
print_metadata_list <- function(metadata_list)
{
  for (file_key in names(metadata_list)) {
    message(file_key)
    metadata <- metadata_list[[file_key]]
    writeLines(kwb.utils::getAttribute(metadata, "first_rows")[1:3])
  }
}
