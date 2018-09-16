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

library(data.tree)
library(igraph)

file <- "~/Desktop/Data/FAKIN/folders_projects/folders_projects_2018-09-11.txt"

paths <- kwb.fakin:::removeCommonRoot(kwb.fakin::read_paths(
  file, encoding = "UTF-8"
))

Encoding(paths) <- "latin1"

path_data <- data.frame(
  pathString = paste0("root/", dirname(paths)),
  leafFolder = basename(paths)
)

View(path_data)

root_node <- data.tree::as.Node(path_data)

root_node$isRoot
root_node$height
root_node$count
root_node$totalCount
root_node$fields
root_node$fieldsAll
root_node$averageBranchingFactor

names(root_node)

subtree <- root_node$GROUNDWATER$PROJECTS $FAKIN

# data.tree --------------------------------------------------------------------

SetGraphStyle(subtree, rankdir = "TB")
SetNodeStyle(subtree$`Data-Work packages`, fillcolor = "LightBlue")

SetNodeStyle(
  subtree, style = "filled,rounded", shape = "box",
  fillcolor = "GreenYellow", fontname = "helvetica",
  tooltip = GetDefaultTooltip
)

plot(as.dendrogram(subtree), center = TRUE)

plot(as.igraph(subtree$Reports, directed = TRUE, direction = "climb"))

# networkD3 --------------------------------------------------------------------

library(networkD3)

network <- ToDataFrameNetwork(subtree, "name")

si*****mpleNetwork(network, fontSize = 10)

subtree <- root_node$GROUNDWATER$PROJECTS

tree_list <- ToListExplicit(subtree, unname = TRUE)

radialNetwork(tree_list)


path_tree <- kwb.fakin:::to_tree(paths)
subtree <- path_tree$SUW_Department$Projects
folder_structure <- kwb.utils::asNoFactorDataFrame(kwb.fakin:::toSubdirMatrix(kwb.fakin:::flatten_tree(subtree)))
View(folder_structure)
folder_structure$id <- seq_len(nrow(folder_structure))

# collapsibleTree --------------------------------------------------------------

collapsibleTree::collapsibleTree(
  folder_structure,
  hierarchy = paste0("V", 1:7),
  width = 800,
  zoomable = TRUE,
  nodeSize = "leafCount",
  collapsed = TRUE
)

shiny::runApp(system.file("examples/02shiny", package = "collapsibleTree"))

# jsTree (Top!) ----------------------------------------------------------------

jsTree::jsTree(kwb.fakin:::flatten_tree(subtree))

# d3Tree -----------------------------------------------------------------------

root <- d3Tree::df2tree(
  rootname = 'kwb-folders',
  struct = folder_structure[1:10, 1:4]
)

d3Tree::d3tree(list(root = root, layout = 'collapse'))

d3Tree::d3tree(list(
  root = d3Tree::df2tree(
    rootname = 'Titanic',
    struct = as.data.frame(Titanic),
    toolTip = letters[1:(ncol(as.data.frame(Titanic)) + 1)]
  ),
  layout = 'collapse'
))

d3Tree::d3tree(list(
  root = d3Tree::df2tree(rootname = 'book', struct = d3Tree::stan.models),
  layout = 'collapse'
))

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

# Get example file list with file properties -----------------------------------

file_paths <- list.files("~/Desktop/Data", full.names = TRUE, recursive = TRUE)

file_info <- file.info(file_paths)

View(file_info)
