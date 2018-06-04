## ------------------------------------------------------------------------
paths <- c(
  "project_1/wp_1/input/file_1.csv",
  "project_1/wp_1/input/file_2.csv",
  "project_1/wp_1/analysis/summary.pdf",
  "project_1/wp_2/input/köpenick_dirty.csv",
  "project_1/wp_2/output/koepenick_clean.csv",
  "project_2/Daten/file_1.csv",
  "project_2/Grafiken/file_1.png",
  "project_2/Berichte/bericht_1.doc",
  "project_2/Berichte/bericht_2.doc"
)

## ------------------------------------------------------------------------
kwb.fakin:::ascii_stats(paths)

## ------------------------------------------------------------------------
kwb.fakin:::subtree_for_treejack(root = "project_1", paths)

## ------------------------------------------------------------------------
parts <- kwb.fakin:::splitPaths(paths)
parts[1:3]

## ------------------------------------------------------------------------
subdirs <- kwb.fakin:::toSubdirMatrix(parts)

## ----results = "asis", echo = FALSE--------------------------------------
knitr::kable(subdirs, caption = "Content of the subdirs matrix")

## ------------------------------------------------------------------------
(parent_paths <- kwb.fakin:::all_path_levels(paths[1]))

parents <- kwb.fakin:::toSubdirMatrix(kwb.fakin:::splitPaths(parent_paths))

## ----results = "asis", echo = FALSE--------------------------------------
knitr::kable(parents, caption = "Content of the parents matrix")

## ------------------------------------------------------------------------
kwb.fakin:::sortedImportance(dirname(paths))

## ------------------------------------------------------------------------
directories <- c("short", "short", "short", "longer/path", "longer/path")

short_dirs <- kwb.fakin:::compress(directories)

as.character(short_dirs)

## ----collapse = TRUE-----------------------------------------------------
(dictionary <- kwb.utils::getAttribute(short_dirs, "dict"))

## ----collapse = TRUE-----------------------------------------------------
(long_dirs <- kwb.utils::resolve(short_dirs, dictionary))

identical(long_dirs, directories)

## ------------------------------------------------------------------------
attributes(kwb.fakin:::compressOneByOne(paths, n = 3))

