paths <- dir("~/Desktop", "\\.R$", recursive = TRUE, full.names = TRUE)

network <- kwb.fakin:::get_path_network(paths, max_depth = 7)

network$nodes$id <- seq_len(nrow(network$nodes))
network$nodes$NodeID <- seq_len(nrow(network$nodes))
network$nodes$label <- network$nodes$name

network$links$from = network$links$source + 1
network$links$to = network$links$target + 1
visNetwork::visNetwork(network$nodes, network$links, width = "100%")

forceNetwork(network$links, network$nodes, NodeID = "id", Group = "name")
simpleNetwork(network$links)

kwb.utils::assignPackageObjects("kwb.fakin")
folder_data <- kwb.utils::asNoFactorDataFrame(
  toSubdirMatrix(kwb.file::split_paths(paths, dbg = FALSE))
)

nodes <- data.frame(id = 1:7, label = paste("Node", 1:7))

edges <- data.frame(
  from = c(1,2,2,2,3,3),
  to = c(2,3,4,5,6,7)
)

visNetwork::visNetwork(nodes, edges, width = "100%")

hc <- hclust(dist(USArrests), "ave")
library(networkD3)
# Create URL. paste0 used purely to keep within line width.
URL <- paste0("https://cdn.rawgit.com/christophergandrud/networkD3/",
              "master/JSONdata//flare.json")

## Convert to list format
Flare <- jsonlite::fromJSON(URL, simplifyDataFrame = FALSE)

## Recreate Bostock example from http://bl.ocks.org/mbostock/4063550
diagonalNetwork(List = Flare, fontSize = 10, opacity = 0.9)

x <- tree1

x <- x[[1]]

is.list(x)

hierarchie <- list(
  name = "ABC",
  children = list(new_node("A"), new_node("B"), new_node("C"))
)

new_node <- function(name) list(name = name)

diagonalNetwork(hierarchie)

hierarchie <- list(
  name = "root",
  children = lapply(names(x), new_node)
)

x <- tree1

diagonalNetwork(get_hierarchical_list(tree))

# get_hierarchical_list --------------------------------------------------------
get_hierarchical_list <- function(x, name = "root")
{
  if (is.list(x)) {
    list(name = name, children = lapply(names(x), function(element) {
      get_hierarchical_list(x[[element]], element)
    }))
  } else {
    list(name = name)
  }
}

URL <- paste0("https://cdn.rawgit.com/christophergandrud/networkD3/",
              "master/JSONdata/miserables.json")
