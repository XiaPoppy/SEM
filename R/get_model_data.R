#' Obtain Data Used in the Formula
#'
#' Return sub-dataset of the original data that contains only the columns
#' used in the dependence formula.
#'
#' @param i an integral number taking from 1 to length(vairable_names).
#' @param adj_matrix the adjacency matrix.
#' @param variable_names vector of variable names.
#' @param data the data containing variable_names, can also be the u-scaled data.
#' @return data of variables in the formula of i-th node.
#' @export
#' @examples
#' library(igraph)
#' i <- 1
#'
#' data('example_data')
#' assem <- example_data
#' assem<- as.data.frame(assem)
#' t1 <- as.data.frame(apply(assem, 2, scale))
#'
#' data('example_edges')
#' df2_tr <- example_edges # edge list
#' edges <- as.matrix(df2_tr)
#' dag_tr <- graph_from_edgelist(edges, directed = TRUE)
#'
#' adjacency_matrix <- get.adjacency(dag_tr, sparse = FALSE)
#' variable_names <- V(dag_tr)$name
#'
#' x <- getModelData(i,adjacency_matrix, variable_names,t1)

getModelData <- function(i,adjacency_matrix, variable_names,data) {

  parents <- variable_names[adjacency_matrix[,i] == 1]
  if (length(parents) > 0) {
    data <- data[c(variable_names[i],parents)]
  } else {
    data <- data[variable_names[i]]
  }

  return(data)
}
