#install.packages("collapsibleTree")
#install.packages("data.tree")
#install.packages("shiny")
#install.packages("treemapify")
#install.packages('jsTree')
#remotes::install_github('metrumresearchgroup/jsTree')
#install.packages("d3Tree")
#devtools::install_github("hrbrmstr/wand")
#install.packages("R.filesets")
#devtools::install_github("kwb-r/kwb.fakin")
Sys.setenv(RSTUDIO_PANDOC = "/usr/bin/pandoc")

# TODO
# - Get file list with properties
#   - which function to use, list.files?
# - test package treemapipfy
# - create example graphs (HTML-pages) for example set of files
#   - using data.table, collapsibleTree, jsTree, treemapify
# - Check promising package R.filesets!
# - Have a look at package RecordLinkage (for similarity of files)

# Load full file information from a text file ----------------------------------

file_info_dir <- "/home/hauke/Desktop/Data/FAKIN/file-info_by-department"
#file_info_dir <- "~/Data/FAKIN"

path_infos <- kwb.fakin:::read_path_information(file_info_dir)

# Get example file list with file properties -----------------------------------

#   user  system elapsed
# 21.492   2.121  26.741

root_dir <- "/home/hauke/Documents/FAKIN"
root_dir <- "/home/hsonne/Desktop"

system.time(file_info <- kwb.fakin::get_recursive_file_info(root_dir))

nrow(file_info)

Encoding(rownames(file_info)) <- "latin1"

# rownames(file_info)[c(3929, 3930, 3936, 3937, 3940)]
# [1] "/aquanes-1/Exchange/01 Health and Safety/01 GEF\xc4HRDUNGSBEURTEILUNGEN
# [3] "/aquanes-1/Exchange/01 Health and Safety/05 Chemikalien/Stoffdatenbl\xe4tter"
# [4] "/aquanes-1/Exchange/01 Health and Safety/06 HSE-Briefing/Sch\xf6nerlinde"
# [5] "/aquanes-1/Exchange/01 Health and Safety/10 Literatur/Gef\xe4hrdungsbeurteilungen"

path_data <- kwb.fakin:::extend_file_info(file_info, n_keep = 2)

# Load paths from a text file --------------------------------------------------

file <- "~/Desktop/Data/FAKIN/folders_projects/folders_projects_2018-09-11.txt"

paths <- kwb.file::remove_common_root(kwb.fakin::read_paths(
  file, encoding = "UTF-8"
))

Encoding(paths) <- "latin1"
path_data <- data.frame(
  pathString = paste0("root/", dirname(paths)),
  leafFolder = basename(paths)
)

# Create a tree with data.tree -------------------------------------------------

test_data <- path_data

system.time(root_node <- data.tree::as.Node(test_data, pathName = "path"))

print(root_node, limit = 10)

root_node$isRoot
root_node$height
root_node$count
root_node$totalCount
root_node$fields
root_node$fieldsAll
root_node$averageBranchingFactor

names(root_node)

#subtree <- root_node$children[[1]]
subtree <- root_node

# treemapify -------------------------------------------------------------------

str(folder_structure)

testdata <- read.table(sep = ",", header = TRUE,
  text = "level_1,level_2,level_3,value,colour
a,b,,10,1
a,c,c1,20,1
a,c,c2,5,2"
)
testdata$label <- kwb.utils::pasteColumns(testdata, paste0("level_", 1:3))
ggplot2::ggplot(testdata, ggplot2::aes(
  area = value, fill = colour, label = label, subgroup = level_2
)) +
  treemapify::geom_treemap() +
  treemapify::geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                  grow = TRUE) +
  treemapify::geom_treemap_subgroup_border(color = "red")

# Plot the tree using data.tree methods ----------------------------------------

# Take care, do not plot large trees!
stopifnot(subtree$totalCount < 20)

plot(subtree)

data.tree::SetGraphStyle(subtree, rankdir = "TB")

marked_node <- subtree$children[[1]]

data.tree::SetNodeStyle(marked_node, fillcolor = "LightBlue", fontcolor = "red")

data.tree::SetNodeStyle(
  subtree, style = "filled,rounded", shape = "box",
  fillcolor = "GreenYellow", fontname = "helvetica",
  tooltip = data.tree::GetDefaultTooltip
)

plot(subtree)

# Plot as dendogram ------------------------------------------------------------

dendogram <- as.dendrogram(subtree)

plot(dendogram, center = TRUE)

# Plot as igraph ---------------------------------------------------------------

node_names <- unname(root_node$Get("name"))
node_ids <- unname(root_node$Get("id"))

# Make the node names unique
#root_node$Set(name = sprintf("%05d__%s", node_ids, node_names))
root_node$Set(name = node_ids)

igraph_tree <- data.tree::as.igraph.Node(root_node)

plot(igraph_tree, directed = TRUE, direction = "climb")

# networkD3: simpleNetwork, radialNetwork --------------------------------------

network <- data.tree::ToDataFrameNetwork(subtree, "size")
networkD3::simpleNetwork(network, fontSize = 6)

tree_list <- data.tree::ToListExplicit(subtree, unname = TRUE)
networkD3::radialNetwork(tree_list)

# collapsibleTree --------------------------------------------------------------

path_tree <- kwb.fakin:::to_tree(path_data$pathString)

subtree <- path_tree

`%>%` <- magrittr::`%>%`

folder_structure <- subtree %>%
  kwb.fakin:::flatten_tree() %>%
  kwb.fakin:::toSubdirMatrix() %>%
  kwb.utils::asNoFactorDataFrame()

folder_structure$id <- seq_len(nrow(folder_structure))

View(folder_structure)

collapsibleTree::collapsibleTree(
  folder_structure,
  hierarchy = c("V1", "V2"),
  width = 800,
  zoomable = TRUE,
  nodeSize = "leafCount",
  collapsed = TRUE
)

shiny::runApp(system.file("examples/02shiny", package = "collapsibleTree"))

# jsTree (Top!) ----------------------------------------------------------------

path_data <- kwb.fakin:::prepare_path_data(path_infos$processing)

js_tree_processing <- jsTree::jsTree(path_data$path, height = "100%")

save(js_tree_processing, file = file.path(tempdir(), "js_tree.RData"))
kwb.utils::hsOpenWindowsExplorer(tempdir())

# d3Tree -----------------------------------------------------------------------

# Example 1
d3Tree::d3tree(list(
  root = d3Tree::df2tree(
    rootname = 'Titanic',
    struct = as.data.frame(Titanic),
    toolTip = letters[1:(ncol(as.data.frame(Titanic)) + 1)]
  ),
  layout = 'collapse'
))

# Example 2
d3Tree::d3tree(list(
  root = d3Tree::df2tree(
    rootname = 'book',
    struct = d3Tree::stan.models
  ),
  layout = 'collapse'
))

# Our tree
d3_tree <- d3Tree::df2tree(
  rootname = 'kwb_example',
  struct = folder_structure
)

d3Tree::d3tree(list(root = d3_tree, layout = 'collapse'))

# wand -------------------------------------------------------------------------

#example_folder <- system.file("extdata", "img", package = "wand")
#example_paths <- list.files(example_paths, full.names = TRUE)

example_paths <- dir("~/Desktop/Data", recursive = TRUE, full.names = TRUE)

length(example_paths)

system.time(properties <- wand::incant(example_paths[1:30]))

# user  system elapsed
# 2.769   0.468   3.247
# => about 1 second per 10 files!!!

sort(table(properties$encoding))

View(properties)

