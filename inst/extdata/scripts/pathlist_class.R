paths <- kwb.fakin:::restore("paths")
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

head(subdirs)

#
# Define a class "pathlist"
#

pathlist <- setClass("pathlist", slots = c(
  folders = "matrix",
  depths = "integer",
  root = "character"
))

setMethod("initialize", "pathlist", function(.Object, paths) {
  stopifnot(is.character(paths))
  all_segments <- kwb.file::split_paths(paths)
  segments <- kwb.file::remove_common_root(all_segments)
  .Object@depths <- lengths(segments)
  .Object@folders <- kwb.file::to_subdir_matrix(segments)
  .Object@root <- kwb.utils::getAttribute(segments, "root")
  .Object
})

setMethod(
  f = "as.character",
  signature = "pathlist",
  definition = function(x, relative = FALSE, i, dbg = FALSE) {
    depths <- x@depths
    print("depths")
    print(depths)
    kwb.utils::catAndRun("Composing path strings", dbg = dbg, {
      folders <- x@folders[i, , drop = FALSE]
      str(folders)
      depths <- x@depths[i]
      paths <- character(nrow(folders))
      for (depth in unique(depths)) {
        message("depth: ", depth)
        indices <- which(depths == depth)
        args <- kwb.utils::asColumnList(
          folders[indices, seq_len(depth), drop = FALSE]
        )
        if (! relative) {
          args <- c(list(x@root), args)
        }
        paths[indices] <- do.call(paste, c(args, sep = "/"))
      }
      paths
    }
    )
  }
)

setMethod("show", "pathlist", function(object) {
  print(as.character(object))
})

setMethod("summary", "pathlist", function(object) {
  cat(sprintf("# Common root: %s\n", object@root))
  cat(sprintf("# Number of paths: %d\n", nrow(object@folders)))
  cat(sprintf("# Max. depth: %d\n", ncol(object@folders)))
  cat("# Top-level paths:")
  print(table(object@folders[, 1]))
  cat("# Paths per depth:")
  print(table(object@depths))
})

setMethod("[", "pathlist", function(x, i, j) {
  folders <- x@folders
  depths <- x@depths
  if (! missing(j) && (min_j <- min(j)) > 1) {
    min_j <- min
    x@root <- paste(x@root, folders[i, seq_len(min_j - 1)])
  }

  x@folders <- folders[i, j, drop = FALSE]
  x@depths <- depths[i]
  x
})

## an object from the class
pathlist1 <- pathlist(paths = paths)
pl <- pathlist1[10:20, 2]
pl@folders
pl@depths

paths2 <- as.character(pathlist1)
paths3 <- as.character(pathlist1, i = 1:100)
identical(paths3, paths[1:100])
head(paths2)
identical(paths, paths2)

summary(pathlist1)

head(pathlist1@folders)

