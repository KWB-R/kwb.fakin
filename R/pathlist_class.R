#
# The class "pathlist" is now implemented in package "pathlist" that is on my
# personal github repository https://github.com/hsonne/pathlist
#

if (FALSE)
{
  paths <- kwb.utils::loadObject("~/Desktop/tmp/paths.RData", "paths")

  length(paths) # 143058

  segments <- kwb.file::split_paths(paths)
  # Splitting paths ... ok. (7.34s)
  system.time(subdirs_2 <- kwb.file::to_subdir_matrix(paths))
  # user  system elapsed
  # 13.864   0.023  14.014
  system.time(subdirs <- kwb.file::to_subdir_matrix(segments))
  # user  system elapsed
  # 5.397   0.004   5.406

  identical(subdirs, subdirs_2) # TRUE

  object.size(paths) / 2^20 # 25.2 MiB
  object.size(segments) / 2^20 # 70.9 MiB
  object.size(subdirs) / 2^20 # 14.7 MiB

  ## an object from the class
  pl_1 <- pathlist(paths = paths)

  class(pl_1)

  pl_1@root
  pl_1@folders
  head(pl_1@depths)

  pl_1[10:20, ]

  paths2 <- as.character(pl_1)
  paths3 <- as.character(pl_1, i = 1:10)

  identical(paths, paths2)
  identical(paths3, paths[1:100])

  pl_1$a
  summary(pl_1$Administratio)
}
