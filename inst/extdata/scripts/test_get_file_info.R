library("kwb.utils")

if (FALSE)
{
  root_dir <- "C:/Users/hsonne/Desktop"

  #============================================================================#
  # Get file properties querying the search index using PowerShell and write
  # them to a CSV file in "<desktop>/tmp"
  #============================================================================#

  system.time(file_info <- kwb.fakin:::get_file_info_powershell(
    root_dir, top_n = NULL
  ))
  # User      System verstrichen
  #12.06       22.94       37.25

  write.table(file_info, sep = ",", row.names = FALSE, file = file.path(
    kwb.utils::desktop(), "tmp", "files_desktop_powershell.csv"
  ))

  #============================================================================#
  # Get file properties using fs::dir_info()
  #============================================================================#

  system.time(file_info_2 <- kwb.fakin::get_recursive_file_info(root_dir))
  # User      System verstrichen
  # 0.94        3.72        8.05

  #============================================================================#
  # Compare the file info tables
  #============================================================================#

  dim(file_info)
  dim(file_info_2)

  #============================================================================#
  # Read CSV files containing file information. The files have been created
  # with PowerShell, direclty on medusa. There are two types of files:
  #
  # 1. path-info-ps-1_<date>_<root_folder>.csv
  # These files have been created with the PowerShell-Command Get-ChildItem
  #
  # 2. path-info-ps-2_<date>_<root_folder>.csv
  # These files have been created by querying the Windows Search Index, just as
  # it is performed within kwb.fakin:::get_file_info_powershell(), see above.
  #============================================================================#

  path <- "//medusa/processing/CONTENTS/file-info_by-department"

  choice <- 1 # Set to 1, 2, or 3 to select department

  dept <- c("GROUNDWATER", "WWT_Department", "SUW_Department")[choice]

  files <- file.path(path, sprintf("path-info-ps-%d_20181116_%s.csv", 1:2, dept))

  file_info_1 <- kwb.fakin:::read_file_info_v2(safePath(files[1]), sep = ",")
  file_info_2 <- kwb.fakin:::read_file_info_search_index(safePath(files[2]))

  View(file_info_1)
  View(file_info_2)

  #============================================================================#
  # Extract the path information from the tables and make them comparable by
  # removing the first common part of all paths
  #============================================================================#

  paths_1 <- kwb.file::remove_common_root(kwb.utils::rStylePath(file_info_1$FullName))
  paths_2 <- kwb.file::remove_common_root(file_info_2$ITEMURL)

  length(paths_1)
  length(paths_2)

  #============================================================================#
  # What files are in 1. but not in 2., i.e. what files are returned by
  # Get-ChildItem but are not captured by the Windows Search Index?
  #============================================================================#

  not_in_index <- ! (paths_1 %in% paths_2)

  table(not_in_index)

  # Show some files that are not in the index
  head(paths_1[not_in_index], 20)

  # What are the extensions of the files that are not in the index?
  sort(table(kwb.utils::fileExtension(paths_1[not_in_index])))

  # Write the files that are not in the index to a text file
  output_file <- file.path(dirname(files[1]), "non_indexed_files.txt")
  writeLines(paths_1[not_in_index], output_file)

  # What different attribute sets occur and are the corresponding files missing
  # in the index or not?
  table(attributes = file_info_1$Attributes, not_in_index)

  # Add a column in 1. indicating if the file is missing in the index or not
  file_info_1$notInIndex <- ifelse(not_in_index, "x", "")

  #============================================================================#
  # What files are in 2. but not in 1., i.e. files that are captured by the
  # Windows Search Index but that are not returned by Get-ChildItem?
  #============================================================================#

  not_found_by_getChildItem <- ! (paths_2 %in% paths_1)

  table(not_found_by_getChildItem)

  # What are the names of the files that are in the Search Index but not
  # returned by Get-ChildItem?
  sort(unique(basename(paths_2[not_found_by_getChildItem])))
}
