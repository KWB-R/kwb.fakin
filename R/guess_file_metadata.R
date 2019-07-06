# guess_file_metadata ----------------------------------------------------------
guess_file_metadata <- function(file, n_first_rows = 1000)
{
  first_rows <- readLines(file, n_first_rows)
  patterns <- c(slash = "/", backslash = "\\\\")
  has_pattern <- lapply(patterns, grepl, first_rows)
  has_paths <- has_pattern$slash | has_pattern$backslash
  separators <- c(",", ";", "\t")

  sep_counts <- sapply(separators, function(sep) sum(stringi::stri_count(
    first_rows, regex = sep
  )))

  sep <- if (all(sep_counts == 0)) "" else separators[which.max(sep_counts)]

  ncol <- 1L

  if (nzchar(sep)) {
    ncol_frequency <- table(lengths(strsplit(first_rows, sep)))
    ncol <- as.integer(names(which.max(ncol_frequency)))
  }

  metadata <- kwb.utils::noFactorDataFrame(
    paths = any(has_paths),
    forbidden = any(grepl("[<>?*]", first_rows)),
    header = if (any(has_paths)) ! has_paths[1] else FALSE,
    windows = any(has_pattern$backslash),
    sep = sep,
    ncol = ncol
  )

  structure(metadata, file = file, first_rows = first_rows)
}
