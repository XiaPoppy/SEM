#' Get Adjacency Matrix and Vairable Names
#'
#' Get the adjacency matrix. Within this function, man can obtain the adjacency
#' matrix from a listdescribing edges from a Graph.
#'
#' @param edge_list a list of 2 columns, directed edges pointing from column 1 to 2.
#' @param directed to tell if the graph is directed or not, default value is TRUE.
#' @param dag_tr an igraph object.
#' @export
#' @examples
#' data('example_edges')
#' df2_tr <- example_edges # edge list
#'
#' get_adj_matrix(df2_tr)

get_adj_matrix <- function(edge_list, directed=TRUE){
#
#

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
#' @examples
#'
#'data('example_edges')
#' df2_tr <- example_edges # edge list
#'
#' dag_tr <- get_igraph_obj(df2_tr)
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
#' @examples
#' library(igraph)
#' data('example_edges')
#' df2_tr <- example_edges # edge list
#' edges <- as.matrix(df2_tr)
#' dag_tr <- graph_from_edgelist(edges, directed = TRUE)
#'
#' get_variable_names(dag_tr)

get_variable_names <- function(dag_tr){

# Parameter description:
# dag_tr: an igraph object
  variable_names <- V(dag_tr)$name

  return(variable_names)
}

