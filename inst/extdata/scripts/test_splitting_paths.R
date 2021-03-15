#
# 1. Source the script to load the functions defined below
#

# Provide random paths ---------------------------------------------------------
if (FALSE)
{
  set.seed(4)

  paths <- kwb.pathdict::random_paths(
    #max_depth = 10,
    max_depth = 5,
    max_elements = 5, depth_to_leaf_weight = function(d) 0.4
  )
}

# Provide real paths -----------------------------------------------------------
if (FALSE)
{
  # Define the path to a file containing a big path list
  file <- "~/pathana-db/path-info_2019-12-01_hauke.csv"

  # Read the big path list
  all_paths <- fakin.path.app:::read_file_paths(file)$path

  # Remove paths of which the encoding changed during pathlist-conversion
  paths <- all_paths[- which_changed(all_paths)]

  kwb.fakin:::store(paths, "test_splitting_paths")
}

# Convert paths to one matrix of subdirectory names ----------------------------
if (FALSE)
{
  length(paths)

  system.time(subdirs_1 <- kwb.file::to_subdir_matrix(paths, method = 1))
  system.time(subdirs_2 <- kwb.file::to_subdir_matrix(paths, method = 2))
  system.time(subdirs_3 <- kwb.file::to_subdir_matrix(paths, method = 3))

  identical(subdirs_1, subdirs_2)
  identical(subdirs_1, subdirs_3)

  View(subdirs_1)
}

# Convert paths to a list of matrices of subdirectory names --------------------
if (FALSE)
{
  # Create list of subdirectory matrices, one per depth level
  subdirs_in_depth <- kwb.file::to_subdir_matrix(paths, result_type = "list")

  # Number of paths in the different depths must sum up to number of all paths
  stopifnot(identical(sum(sapply(subdirs_in_depth, nrow)), length(paths)))

  # Restore the paths from the list of subdirectory matrices
  system.time(backpaths_1 <- restore_paths(subdirs_in_depth))

  # Compare with original paths
  identical(backpaths_1, paths)

  # Create a pathlist object
  pl <- pathlist::pathlist(paths)

  # Amount of empty space in the subdirectory matrix
  mean(! nzchar(pl@folders))

  # Restore the paths from the pathlist object
  system.time(backpaths_2 <- as.character(pl))

  # Compare with original paths
  identical(backpaths_2, paths)

  # Compare the sizes of both data structures
  kwb.utils::percentage(size_mb(subdirs_in_depth), size_mb(pl@folders))

  # Define depth of which to select paths
  depth <- 4

  # Select paths in a certain depth from the path object
  tmp_paths_1 <- pl[which(pl@depths + lengths(strsplit(pl@root, "/")) == depth)]

  # Select paths in a certain depth from the list structure
  tmp_paths_2 <- restore_paths(subdirs_in_depth, keys = as.character(depth))

  # Compare the selected path sets
  identical(as.character(tmp_paths_1), tmp_paths_2)

  # Look into the function definition of a class method and a package function
  getMethod("as.character", "pathlist")
  pathlist:::paste_segments
}

# Create node information from paths -------------------------------------------
if (FALSE)
{
  # Split original paths at slashes
  system.time(slashes <- gregexpr("/", paths))

  # Compare performance with kwb.file::to_subdir_matrix()
  system.time(subdirs <- kwb.file::to_subdir_matrix(paths))

  # Calculate path depths
  depths <- lengths(slashes) + 1

  nodes <- lapply(seq_len(max(depths)), function(depth) {

    print(depth)
    #depth <- 3
    indices <- which(depths >= depth)

    x <- sapply(indices, function(i) {
      if (depth < depths[i]) {
        substr(paths[i], 1, slashes[[i]][depth] - 1)
      } else {
        paths[i]
      }
    })

    unique(x)
  })
}

# Create node information from list of subdirectory matrices -------------------
if (FALSE)
{
  leaves <- lapply(subdirs_in_depth, function(x) x[, ncol(x)])

  depths <- as.integer(names(subdirs_in_depth))[-1]

  for (depth in depths) {

    #depth <- depths[1]
    print(depth)

    keys <- list(me = as.character(depth), parent = as.character(depth - 1L))

    subdirs <- subdirs_in_depth[[keys$me]]

    parent_paths <- rowwise_paths(subdirs[, -depth, drop = FALSE])

    parent_indices <- match(parent_paths, leaves[[keys$parent]])

    attr(leaves[[keys$me]], "parent") <- parent_indices
  }

  str(leaves)
}

# which_changed ----------------------------------------------------------------
which_changed <- function(paths)
{
  pl <- pathlist:::pathlist(paths)

  reconstructed_paths <- as.character(pl)

  has_changed <- reconstructed_paths != paths

  if (! any(has_changed)) {
    return(integer())
  }

  which_changed <- which(has_changed)

  message(
    "These ", sum(has_changed), " elements differ:\n",
    paste0("[", which_changed, "]: ", paths[has_changed], collapse = "\n")
  )

  which_changed
}

# restore_paths ----------------------------------------------------------------
restore_paths <- function(subdirs_in_depth, keys = names(subdirs_in_depth))
{
  stopifnot(all(keys %in% names(subdirs_in_depth)))

  rows <- kwb.utils::getAttribute(subdirs_in_depth, "original_rows")

  x <- lapply(subdirs_in_depth[keys], function(m) do.call(paste, c(
    kwb.utils::asColumnList(m), sep = "/"
  )))

  unlist(x, use.names = FALSE)[order(unlist(rows[keys]))]
}

# rowwise_paths ----------------------------------------------------------------
rowwise_paths <- function(m)
{
  #m <- subdirs_in_depth$`1`

  paths <- do.call(paste, c(kwb.utils::asColumnList(m), sep = "/"))

  stats::setNames(paths, rownames(m))
}

# size_mb ----------------------------------------------------------------------
size_mb <- function(x)
{
  as.integer(object.size(x)) / 2^20
}
