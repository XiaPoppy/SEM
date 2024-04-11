#' Separate Unconnected Graph
#'
#' Want to know whether a graph is connected or not. If unconnected, I want to be able
#' to get a subgraph which contains a certain node we give.
#'
#' @param edgelist a list describing the edges from a graph.
#' @param directed true or false.
#' @param node_name the name of the variable.
#' @return an edge-list from a subgraph containing the given node i(if possible).
#' @export
#' @examples
#' #
#'

get_subedgelist <- function(node_name, edgelist,directed=TRUE){
  graph <- graph_from_edgelist(edge_list, directed = directed)
  if(is.connected(graph)){
    print('This graph is connected')
    return(edge_list)
  }else{
    print('This graph is unconnected')
    sub_edgelist <- list()#initialize
    #########TODO###################


    ################################
    return(sub_edgelist)
  }
}
