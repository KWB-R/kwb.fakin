# data_frame_to_paths ----------------------------------------------------------
data_frame_to_paths <- function(df)
{
  # Paste columns and remove all trailing slashes
  gsub("/+$", "", kwb.utils::pasteColumns(df, sep = "/"))
}

# get_max_path_width -----------------------------------------------------------
get_max_path_width <- function(paths)
{
  max(colSums(toSubdirMatrix(paths) != ""))
}

# get_paths_to_latest_content_files --------------------------------------------
get_paths_to_latest_content_files <- function(month_string = "2018-12")
{
  content_dir <- Sys.getenv("FAKIN_CONTENTS")

  if (content_dir == "") {

    stop_(
      "Please use Sys.setenv() to set the environment variable ",
      "'FAKIN_CONTENTS' to the path to the folder containing the content files."
    )
  }

  content_files <- dir(full.names = TRUE, file.path(content_dir, month_string))

  last_date <- sort_unique(stringr::str_extract(content_files, "\\d{8}"))[1]

  content_files[grepl(last_date, content_files)]
}

# getSubdirsByFrequence --------------------------------------------------------
getSubdirsByFrequence <- function(subdirs, cumid, freqinfo, dbg = TRUE)
{
  kwb.utils::printIf(dbg, freqinfo)

  rows <- which(cumid[, freqinfo$depth] == freqinfo$n.x)[1]

  subdirs[rows, seq_len(freqinfo$depth)]
}

# lookup -----------------------------------------------------------------------
lookup <- function(x, dict)
{
  #x <- old_dirs; dict <- old_dict

  ready <- x %in% toPlaceholder(names(dict))

  out <- x

  out[! ready] <- toPlaceholder(names(dict[match(x[! ready], dict)]))

  out
}

# removeCommonRoot -------------------------------------------------------------

#' Remove the common root parts
#'
#' @param x list of vectors of character as returned by
#'   \code{\link[base]{strsplit}} or a vector of character.
#' @param n_keep number of common path segments to keep (so that the path
#'   tree keeps its root)
#' @param dbg if \code{TRUE} debug messages are shown
#'
#' @export
#' @examples
#' # Split paths at the slashes
#' absparts <- strsplit(c("a/b/c", "a/b/d", "a/b/e/f/g", "a/b/hi"), "/")
#'
#' # Remove the common parts of the paths
#' relparts <- removeCommonRoot(absparts)
#' relparts
#'
#' # The extracted root is returned in attribute "root"
#' attr(relparts, "root")
#'
removeCommonRoot <- function(x, n_keep = 0, dbg = TRUE)
{
  use_function_instead(kwb.file::remove_common_root, kwb.fakin::removeCommonRoot)
  kwb.file::remove_common_root(x, n_keep, dbg)
}

# replaceSubdirs ---------------------------------------------------------------
replaceSubdirs <- function(s, r, p)
{
  selected <- startsWithParts(s, r)

  cols <- seq(length(r) + 1, ncol(s))

  fillright <- matrix(nrow = sum(selected), ncol = length(r) -1)

  s[selected, ] <- cbind(p, s[selected, cols, drop = FALSE], fillright)

  # Remove empty columns
  maxcol <- max(which(apply(s, 2, function(x) sum(! is.na(x))) > 0))

  s[, seq_len(maxcol)]
}

# splitPaths -------------------------------------------------------------------
splitPaths <- function(paths, dbg = TRUE)
{
  use_function_instead(kwb.file::split_paths, splitPaths)
  kwb.file::split_paths(paths)
}

# startsWithParts --------------------------------------------------------------

#' Do Subfolder List Elements Start with Given Folder Names?
#'
#' @param parts list of list of character as returned by
#'   \code{\link[kwb.file]{split_paths}} or a matrix of character representing
#'   the subfolder names at the different folder depths as returned by
#'   \code{\link{toSubdirMatrix}}.
#' @param elements vector of character giving the sequence of strings to be
#'   found in \code{parts}
#' @return vector of logical as long as \code{parts} containing \code{TRUE} at
#'   positions \code{i} for which \code{all(parts[[i]][seq_along(elements)] ==
#'   elements)} is \code{TRUE}
#'
#' @export
#'
#' @examples
#' parts <- strsplit(c("a/b/c", "a/b/d", "b/c"), "/")
#' startsWithParts(parts, elements = c("a", "b"))
#' startsWithParts(parts, elements = c("b", "c"))
#'
#' subdir_matrix <- kwb.fakin::toSubdirMatrix(parts)
#' startsWithParts(subdir_matrix, elements = c("a", "b"))
#' startsWithParts(subdir_matrix, elements = c("b", "c"))
#'
startsWithParts <- function(parts, elements)
{
  stopifnot(is.list(parts) || is.matrix(parts))

  stopifnot(all(! is.na(elements)))

  length_out <- if (is.list(parts)) length(parts) else nrow(parts)

  selected_at_level <- lapply(seq_along(elements), function(i) {

    if (is.list(parts)) {

      sapply(parts, "[", i) == elements[i]

    } else {

      ! is.na(parts[, i]) & (parts[, i] == elements[i])
    }
  })

  Reduce(`&`, selected_at_level, init = rep(TRUE, length_out))
}


# toPlaceholder ----------------------------------------------------------------
toPlaceholder <- function(x)
{
  paste0("<", x, ">")
}
