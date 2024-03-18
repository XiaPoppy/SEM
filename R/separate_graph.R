# in this file, functions is used to detect if it is connected,
# and get edgelist from a subgraph containing node i(if possible)

is_connect <- function(edgelist, directed=TRUE){
# return: TRUE or FALSE value, is connected graph or not
  graph <- graph_from_edgelist(edge_list, directed = directed)

  return(is.connected(graph))
}

get_subedgelist <- function(node_name, edgelist){
  sub_edgelist <- list()#initialize

  return(sub_edgelist)
}
