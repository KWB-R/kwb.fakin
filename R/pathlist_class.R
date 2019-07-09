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

# paste_subdirs ----------------------------------------------------------------
paste_subdirs <- function(folders, depths, root = NULL)
{
  paths <- character(nrow(folders))
  for (depth in unique(depths)) {
    indices <- which(depths == depth)
    args <- kwb.utils::asColumnList(
      folders[indices, seq_len(depth), drop = FALSE]
    )
    if (! is.null(root)) {
      args <- c(list(root), args)
    }
    paths[indices] <- do.call(paste, c(args, sep = "/"))
  }
  paths
}

setMethod(
  f = "as.character",
  signature = "pathlist",
  definition = function(x, relative = FALSE, i, dbg = FALSE) {
    paths <- kwb.utils::catAndRun("Composing path strings", dbg = dbg, {
      paste_subdirs(
        folders = x@folders[i, , drop = FALSE],
        depths = x@depths[i],
        root = if (! relative) x@root
      )
    })
  }
)

setMethod("show", "pathlist", function(object) {
  #n <- 10
  #print(head(as.character(object), n))
  #cat(sprintf("[%d omitted]\n", length(object@depths) - n))
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

setMethod("$", "pathlist", function(x, name) {
  folders <- x@folders
  folders_1 <- folders[, 1]
  top_level_folders <- unique(folders_1)
  if (! name %in% top_level_folders) {
    stop(
      "No such top-level folder: '", name, "'. Available folders: ",
      kwb.utils::stringList(top_level_folders), call. = FALSE
    )
  }
  keep <- folders_1 == name
  x@folders <- folders[keep, -1]
  x@depths <- x@depths[keep] + 1L
  x@root <- paste0(x@root, "/", name)
  x
})

setGeneric(".DollarNames")

.DollarNames.pathlist <- function(x, pattern) {
  stop("not implemented")
  #grep(pattern, x@folders[, 1], value = TRUE)
}

setMethod(".DollarNames", "pathlist", .DollarNames.pathlist)
