path_data <- path_infos$SUW_Department
x <- kwb.fakin:::prepare_for_treemap(path_data, n_keep = 1)
x$size[x$size == 0] <- 1
max_levels <- min(which(is.na(x[grep("^level", names(x))]), arr.ind = TRUE)[, 2])

for (n_levels in seq_len(max_levels)) {
  treemap::treemap(x, index = names(x)[1:n_levels], vSize = "size")
}

head(folder_data)

treemap::treemap(folder_data, index = paste0("level_", 1:1), vSize = "size")
treemap::treemap(folder_data, index = paste0("level_", 1:2), vSize = "size")
treemap::treemap(folder_data, index = paste0("level_", 1:3), vSize = "size")
treemap::treemap(folder_data, index = paste0("level_", 1:4), vSize = "size")

example_data <- kwb.utils::noFactorDataFrame(
  v1 = c("A", "A", "A", "A", "A", "B"),
  v2 = c("a1", "a1", "a2", "a2", "a3", NA),
  v3 = c("a1x", "a1y", "a2x", "a2y", NA, NA),
  size = c(rep(10, 5), 20),
  files = 1
)

treemap::treemap(example_data, index = "v1", vSize = "size")

treemap::treemap(example_data, index = paste0("v", 1:2), vSize = "size")
treemap::treemap(example_data, index = paste0("v", 1:2), vSize = "size", vColor = "files")

treemap::treemap(example_data, index = paste0("v", 1:3), vSize = "size")

treemap::treemap(example_data, index = paste0("v", 1:3), vSize = "size",
                 type = "depth", vColor = "files")

treemap::treemap(example_data, index = paste0("v", 1:3), vSize = "value",
                 type = "value", vColor = "value")

treemap::treemap(example_data, index = paste0("v", 1:2), vSize = "value",
                 type = "value", vColor = "value")
