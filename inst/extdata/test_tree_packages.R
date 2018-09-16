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

# Get example file list with file properties -----------------------------------

#   user  system elapsed
# 21.492   2.121  26.741
system.time(
  file_info <- kwb.fakin::get_recursive_file_info(root_dir = "~/Desktop/test_diff")
)

Encoding(rownames(file_info)) <- "latin1"

rownames(file_info)[c(3929, 3930, 3936, 3937, 3940)]

# [1] "/aquanes-1/Exchange/01 Health and Safety/01 GEF\xc4HRDUNGSBEURTEILUNGEN
# [3] "/aquanes-1/Exchange/01 Health and Safety/05 Chemikalien/Stoffdatenbl\xe4tter"
# [4] "/aquanes-1/Exchange/01 Health and Safety/06 HSE-Briefing/Sch\xf6nerlinde"
# [5] "/aquanes-1/Exchange/01 Health and Safety/10 Literatur/Gef\xe4hrdungsbeurteilungen"

full_file_info <- kwb.fakin:::add_path_column(file_info)

View(full_file_info)

system.time(full_tree <- data.tree::as.Node(full_file_info))

full_tree

print(full_tree$tmp$furain$raw_images$`2017`$`201701`)

full_tree$fields

# Load paths from a text file --------------------------------------------------

#library(data.tree)
#library(igraph)

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

# treemapify
str(folder_structure)

ggplot2::ggplot(folder_structure, ggplot2::aes(
  area = gdp_mil_usd, fill = hdi)) +
  treemapify::geom_treemap()

# data.tree --------------------------------------------------------------------

subtree <- full_tree

data.tree::SetGraphStyle(subtree, rankdir = "TB")
data.tree::SetNodeStyle(subtree$`aquanes-1`, fillcolor = "LightBlue")
data.tree::SetNodeStyle(
  subtree, style = "filled,rounded", shape = "box",
  fillcolor = "GreenYellow", fontname = "helvetica",
  tooltip = data.tree::GetDefaultTooltip
)

plot(as.dendrogram(subtree$`aquanes-1`$Exchange$`03 Versuchsanlage WP1`), center = TRUE)

full_tree$Set(name = sprintf("%05d__%s", seq_len(full_tree$totalCount), full_tree$Get("name")))

plot(data.tree::as.igraph.Node(full_tree$`00002__aquanes-1`$`00003__Administration`, directed = TRUE, direction = "climb"))

# networkD3 --------------------------------------------------------------------

library(networkD3)

subtree <- full_tree$`00002__aquanes-1`$`00097__Communication`

network <- ToDataFrameNetwork(subtree, "size")

head(network)

networkD3::simpleNetwork(network, fontSize = 6)

tree_list <- ToListExplicit(subtree, unname = TRUE)

networkD3::radialNetwork(tree_list)

subdata <- as.data.frame(subtree)

View(subdata)

path_tree <- kwb.fakin:::to_tree(paths)
subtree <- path_tree$SUW_Department$Projects

folder_structure <- kwb.utils::asNoFactorDataFrame(kwb.fakin:::toSubdirMatrix(
  kwb.fakin:::flatten_tree(subtree)
))

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

jsTree::jsTree(full_file_info$pathString[1:10])

x <- table(full_file_info$pathString)

# d3Tree -----------------------------------------------------------------------

folder_structure <- kwb.fakin:::toSubdirMatrix(full_file_info$pathString)

folder_structure <- kwb.utils::asNoFactorDataFrame(folder_structure)

View(folder_structure)

root <- d3Tree::df2tree(
  rootname = 'kwb-folders',
  struct = folder_structure[1:10, 1:10]
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
  root = d3Tree::df2tree(
    rootname = 'book',
    struct = d3Tree::stan.models
  ),
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

