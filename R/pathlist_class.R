#
# The class "pathlist" is now implemented in package "pathlist" that is on my
# personal github repository https://github.com/hsonne/pathlist
#

library(pathlist)

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

  to_mib <- function(x) as.numeric(object.size(x)) / 2^20
  to_mib(paths) # 25.2 MiB
  to_mib(segments) # 70.9 MiB
  to_mib(subdirs) # 14.7 MiB

  ## an object from the class
  pl_1 <- pathlist(paths = paths)

  class(pl_1)

  pl_1@root
  dim(pl_1@folders)
  head(pl_1@depths)

  pl_1[10:20, ]

  paths2 <- as.character(pl_1)

  identical(paths, paths2)

  summary(pl_1$Projects)
  summary(pl_1$Administration)
}
