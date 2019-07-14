#
# The class "pathlist" is now implemented in package "pathlist" that is on my
# personal github repository https://github.com/hsonne/pathlist
#

if (FALSE)
{
  #library(pathlist)

  if (kwb.utils::user() == "hauke") {
    paths <- kwb.utils::loadObject("~/Desktop/tmp/paths.RData", "paths")
  } else {
    path_info <- kwb.utils::loadObject(
      "~/../Desktop/tmp/path-info_suw.RData", "path_info"
    )
    paths <- path_info$path
  }

  length(paths) # 143058

  # Create an object of class pathlist
  pl <- pathlist::pathlist(paths = paths)

  # Can we use functions for plotting trees?
  system.time(tree <- kwb.fakin:::to_tree(paths))
  system.time(tree2 <- kwb.fakin:::to_tree2(paths))
  str(tree, 1)
  str(tree2, 2)

  tree$"Y:"$WWT_Department
  tree2$


  folder_data <- kwb.utils::asNoFactorDataFrame(pl@folders)
  View(kwb.fakin:::get_links_at_depth(2, folder_data))

  files <- pl@basename()
  freq <- table(files)
  which.max(freq)
  pl[grep("KAWestewitz.csv", files)]

  class(pl)

  pl@root
  dim(pl@folders)
  head(pl@depths)

  head(pl)

  setAs(pl, "character")
  pathlist::as.list(head(pl))

  pl[5:10]
  as.character(pl[5:10])

  paths2 <- as.character(pl)

  identical(paths, paths2)

  pathlist::summary(pl)
  pathlist::summary(pl$Projects)
}

# Compare two versions of tree generation --------------------------------------
if (FALSE)
{
  #
  # to_tree2() internally uses the pathlist class
  #

  #paths <- c("r/a", "r/a/b", "r/b", "r/b/c/d")
  system.time(tree1 <- kwb.fakin:::to_tree(paths))
  system.time(tree2 <- kwb.fakin:::to_tree2(paths))

  identical(tree1, tree2)

  out1 <- capture.output(str(tree1))
  out2 <- capture.output(str(tree2))
  length(out1)
  length(out2)
  print(tree1, 5)
  print(tree2, 5)
}

# Compare sizes of different structures ----------------------------------------
if (FALSE)
{
  segments <- kwb.file::split_paths(paths)
  # Splitting paths ... ok. (7.34s)
  system.time(subdirs_2 <- kwb.file::to_subdir_matrix(paths))
  # user  system elapsed
  # 13.864   0.023  14.014
  system.time(subdirs <- kwb.file::to_subdir_matrix(segments))
  # user  system elapsed
  # 5.397   0.004   5.406

  identical(subdirs, subdirs_2) # TRUE

  to_mib <- function(x) as.numeric(object.size(x)) / 2^20
  to_mib(paths) # 25.2 MiB
  to_mib(segments) # 70.9 MiB
  to_mib(subdirs) # 14.7 MiB
}
