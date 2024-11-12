#' Get Adjacency Matrix and Vairable Names
#'
#' Get the adjacency matrix.
#'
#' @param edge_list a list of 2 columns, directed edges pointing from column 1 to 2.
#' @param directed to tell if the graph is directed or not, default value is TRUE.
#' @param dag_tr an igraph object.
#' @export
#' @examples
#' edge_list <- list(c("b", "c"), c("a", "b"))
#' directed <- TRUE
#'
#' get_adj_matrix(edge_list, directed)
#' dag_tr <- get_igraph_obj(edge_list, directed)
#' get_variable_names(dag_tr)

get_adj_matrix <- function(edge_list, directed=TRUE){
# Within this function, man can obtain the adjacency matrix from a list
# describing edges from a Graph

# Parameter description:
# edge_list: a list of 2 columns, directed edges pointing from column 1 to 2
# directed: to tell if the graph is directed or not, default value is TRUE

  edges <- as.matrix(edge_list)
  dag_tr <- graph_from_edgelist(edges, directed = directed)
  adjacency_matrix <- get.adjacency(dag_tr, sparse = FALSE)

  return(adjacency_matrix)
}
#' Get Adjacency Matrix and Vairable Names
#'
#' Get an igraph object from a given edge list.
#'
#' @param edge_list a list of 2 columns, directed edges pointing from column 1 to 2.
#' @param directed to tell if the graph is directed or not, default value is TRUE.
#' @export
get_igraph_obj <- function(edge_list,directed=TRUE){

  edges <- as.matrix(edge_list)
  dag_tr <- graph_from_edgelist(edges, directed = directed)

  return(dag_tr)
}

#' Get Adjacency Matrix and Vairable Names
#'
#' Get variable names from an igraph object.
#'
#' @param dag_tr an igraph object.
#' @export
get_variable_names <- function(dag_tr){

# Parameter description:
# dag_tr: an igraph object
  variable_names <- V(dag_tr)$name

  return(variable_names)
}

