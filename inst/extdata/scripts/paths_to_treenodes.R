# Define some random paths
paths <- kwb.pathdict:::random_paths()

# Convert paths to a matrix of subdirectory names
x <- kwb.file::to_subdir_matrix(paths)
#x <- pl@folders

# Number of paths
nrow(x)

# Which fields are not empty
is_set <- matrix(nzchar(x), nrow = nrow(x), ncol = ncol(x))

# Initialise a matrix of (tree node) IDs
ids <- matrix(integer(), nrow = nrow(x), ncol = ncol(x))

# Define function to generate IDs
generate_ids <- function(xx) {
  if (is.matrix(xx)) {
    xx <- do.call(paste, c(kwb.utils::asColumnList(xx), sep = "-"))
  }
  match(xx, unique(xx))
}

# Define variable to store the maximum ID so far
max_id <- 0

# Loop through the columns of the subdirectory matrix
for (j in seq_len(ncol(x))) {

  message("Giving IDs to nodes in depth ", j, "/", ncol(x))

  rows <- which(is_set[, j])
  temp_ids <- generate_ids(x[rows, j])

  if (j > 1) {
    temp_ids <- generate_ids(xx = cbind(ids[rows, seq_len(j - 1)], temp_ids))
  }

  ids[rows, j] <- temp_ids + max_id
  max_id <- max_id + max(temp_ids)
}

View(ids)

words <- sort(unique(x[nzchar(x)]))

indices <- which(is_set, arr.ind = TRUE)

id2word[ids[indices]] <- x[indices]

head(id2word)

has_child <- do.call(rbind, lapply(seq(n, 2, by = -1), function(j) {
  ids[which(! is.na(ids[, j])), c(j - 1, j)]
}))

max_id <- max(ids, na.rm = TRUE)

is_identical <- list()

id_in_col <- match(x[, 1], unique(x[, 1]))

lapply(split(seq_along(id_in_col), id_in_col), function(indices) {
  ids[indices, 1]
})

matrix(id2word[tail(has_child)], ncol = 2)

length(words)
head(words)
tail(words)

m <- matrix(match(x, words), nrow = nrow(x), ncol = ncol(x))

View(m)
