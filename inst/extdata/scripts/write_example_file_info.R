#
# purpose: This script provides example files that we can use in the vignettes
#   and examples in the inst/extdata folder of the package
#
# author: Hauke Sonnenberg
# created: 2019-06-29
#

library(dplyr)

paths <- kwb.pathdict::random_paths()
n_files <- length(paths)
seconds_per_year <- 60 * 60 * 24 * 365
n_sample <- function(x) sample(x, size = n_files, replace = TRUE)

file_info <- kwb.utils::noFactorDataFrame(
  path = paths,
  type = "file",
  size = n_sample(2^30),
  last_access = as.POSIXct("2008-05-15") + n_sample(10 * seconds_per_year)
)

dir_info <- file_info %>%
  mutate(path = dirname(path)) %>%
  group_by(path) %>%
  summarise(last_access = max(last_access)) %>%
  mutate(size = 0, type = "directory")

full_info <- bind_rows(file_info, dir_info) %>%
  arrange(path)

View(full_info)

stopifnot(basename(getwd()) == "kwb.fakin")

kwb.fakin::write_csv(full_info, "inst/extdata/example_file_info_1.csv")
