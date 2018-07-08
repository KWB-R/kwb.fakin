# subtree_for_treejack ---------------------------------------------------------
subtree_for_treejack <- function(
  root, paths = NULL, file = NULL,
  encoding = c("Windows-1252", "UTF-8", "ISO-8859-1")[1],
  stdout = is.null(output_file), output_file = NULL
)
{
  # root must end in "/"
  root <- check_or_set_ending_slash(root)

  # If no file is given, read paths based on root
  if (is.null(paths) && is.null(file)) {

    cat("Getting paths based on root path", root, "... ")
    filepaths <- dir(root, recursive = TRUE, no.. = TRUE)
    cat("ok.\n")

  } else {

    if (is.null(paths)) {

      # Read the paths from the file
      cat("Reading paths from", file, "...\n")
      paths <- readLines(file, encoding = encoding)
      cat("  ok. Number of paths:", length(paths), "\n")
    }

    # Filter for paths based on root and remove the root
    cat("Filter for paths starting with", root, "...\n")
    paths <- paths[grepl(root, paths, fixed = TRUE)]
    cat("  ok. Remaining paths:", length(paths), "\n")

    cat("Convert to relative paths ... ")
    paths <- gsub(root, "", paths, fixed = TRUE)
    cat("ok.\n")
  }

  # We are only interested in the folders, not the files
  dirpaths <- unique(dirname(paths))

  # Remove "." or ".."
  dirpaths <- dirpaths[! isDotOrDoubleDot(dirpaths)]

  # We need all levels to a "leaf" subdirectory
  dirpaths <- sort(unique(unlist(lapply(dirpaths, all_path_levels))))

  subdirs <- toSubdirMatrix(splitPaths(sort(dirpaths)))

  # Keep only the last entry of each directory (required for import to Treejack)
  leafs <- to_leaf_matrix(subdirs)

  textlines <- apply(leafs, 1, function(x) {
    kwb.utils::collapsed(kwb.utils::defaultIfNA(x, ""), collapse = "\t")
  })

  if (stdout) {

    cat("\n")
    cat("================================================================\n")
    cat("\n")
    cat("Copy and paste the output below to the 'Bulk import' input field\n")
    cat("at https://www.optimalworkshop.com\n")
    cat("\n")
    cat("================================================================\n")
    cat("\n")

    kwb.utils::catLines(textlines)

  } else {

    if (is.null(output_file)) {

      textlines

    } else {

      kwb.utils::writeText(textlines, output_file, "tree structure to")
    }
  }
}

# check_or_set_ending_slash ----------------------------------------------------
check_or_set_ending_slash <- function(x)
{
  # Add slash to the end and replace multiple occurrences of slash at the end
  gsub("/+$", "/", paste0(x, "/"))
}

# all_path_levels --------------------------------------------------------------

#' All Paths to Parent Folders
#'
#' For a given path \code{a/b/c/d}, all the parent paths \code{a}, \code{a/b},
#' \code{a/b/c} and the path itself (\code{a/b/c/d}) are returned.
#'
#' @param path one character string representing a file path
#' @return vector of character representing all parent paths and the \code{path}
#'   itself as the last element
#'
#' @examples
#' paths <- kwb.fakin:::all_path_levels("this/is/a/long/path")
#' kwb.fakin:::toSubdirMatrix(kwb.fakin:::splitPaths(paths))
#'
all_path_levels <- function(path)
{
  parts <- strsplit(path, "/")[[1]]

  sapply(seq_along(parts), function(i) {

    paste(parts[seq_len(i)], collapse = "/")
  })
}

# get_example_leafs ------------------------------------------------------------
get_example_leafs <- function(size = 10, depth = 5)
{
  stopifnot(depth <= length(LETTERS))
  stopifnot(size < 1000)

  indentation <- function(depth, sep = "\t") {

    do.call(paste0, as.list(rep(sep, depth)))
  }

  folderInDepth <- function(i, prefix = "") {

    paste0(indentation(i-1), prefix, LETTERS[i])
  }

  unlist(lapply(sprintf("T%02d_", seq_len(size)), function(prefix) {

    sapply(seq_len(depth), folderInDepth, prefix = prefix)
  }))
}

# to_leaf_matrix ---------------------------------------------------------------
to_leaf_matrix <- function(subdirs)
{
  t(apply(subdirs, 1, function(row) {

    i_last <- max(which(row != ""))

    if (i_last > 1) {

      row[seq(1, i_last - 1)] <- NA
    }

    if (i_last < ncol(subdirs)) {

      row[seq(i_last + 1, ncol(subdirs))] <- NA
    }

    row
  }))
}
