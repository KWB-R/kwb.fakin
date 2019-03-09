library(kwb.utils)

# MAIN: Understanding Encodings ------------------------------------------------
if (FALSE)
{
  file <- safePath(desktop(), "which-encoding")

  readLines(file)

  raw_chars <- readBin(file, what = "raw", n = 100)

  kwb.fakin:::get_special_character_info(rawToChar(raw_chars[68:80]))
}

# MAIN: Plot Path Network ------------------------------------------------------
if (FALSE)
{
  # If we need to debug functions from kwb.fakin
  # kwb.utils::assignPackageObjects("kwb.fakin")

  # Get the paths to all folders on the desktop
  paths_desktop_tmp <- dir(safePath(desktop(), "tmp"), recursive = TRUE)
  paths_desktop <- dir(safePath(desktop()), recursive = TRUE)

  dirs_desktop <- dirname(paths_desktop)
  dirs_desktop_no_tmp <- dirs_desktop[! grepl("tmp/", dirs_desktop)]

  # Define very simple paths
  paths_simple <- c(
    "a",
    "a/c/e",
    "a/c/e",
    "a/d",
    "b/c",
    "b"
  )

  # Set the paths to be plotted
  paths <- dirs_desktop_no_tmp
  paths <- paths_simple

  # How many paths?
  length(paths)

  # First unique paths
  head(sort(unique(paths)))

  # Print all paths
  catLines(paths)

  # Plot the folder network
  plot_path_network(paths, max_depth = 2, fontSize = 12)

  # Can we use functions for plotting trees?
  kwb.fakin:::to_tree(x)
}
