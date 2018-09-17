#install.packages("collapsibleTree")
#install.packages("data.tree")
#install.packages("shiny")
#install.packages("treemapify")
#install.packages('jsTree')
#remotes::install_github('metrumresearchgroup/jsTree')
#install.packages("d3Tree")
#devtools::install_github("hrbrmstr/wand")
#install.packages("R.filesets")

# TODO
# - Get file list with properties
#   - which function to use, list.files?
# - test package treemapipfy
# - create example graphs (HTML-pages) for example set of files
#   - using data.table, collapsibleTree, jsTree, treemapify
# - Check promising package R.filesets!
# - Have a look at package RecordLinkage (for similarity of files)

`%>%` <- magrittr::`%>%`

# Get example file list with file properties -----------------------------------

#   user  system elapsed
# 21.492   2.121  26.741
root_dir <- "/home/hauke/Documents/FAKIN"

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

paths <- kwb.fakin:::removeCommonRoot(kwb.fakin::read_paths(
  file, encoding = "UTF-8"
))

Encoding(paths) <- "latin1"

path_data <- data.frame(
  pathString = paste0("root/", dirname(paths)),
  leafFolder = basename(paths)
)

# Create a tree with data.tree -------------------------------------------------

View(path_data)

system.time(root_node <- data.tree::as.Node(path_data))

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

ggplot2::ggplot(folder_structure, ggplot2::aes(
  area = gdp_mil_usd, fill = hdi)) +
  treemapify::geom_treemap()

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

jsTree::jsTree(path_data$pathString[1:100])

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
