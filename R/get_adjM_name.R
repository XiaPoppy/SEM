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

get_variable_names <- function(dag_tr){

# Parameter description:
# dag_tr: an igraph object
  variable_names <- V(dag_tr)$name

  return(variable_names)
}

get_igraph_obj <- function(edge_list){

  edges <- as.matrix(edge_list)
  dag_tr <- graph_from_edgelist(edges, directed = directed)

  return(dag_tr)
}
