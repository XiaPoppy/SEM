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
#' i <- 1
#' adj_mat <- matrix(c(0,0,0,1,0,0,0,1,0), nrow=3, ncol=3, byrow=TRUE)
#' variable_names <- c('x1','x2','x3')
#' data <- data.frame(x1=rnorm(10,0,1),x2=rnorm(10,0,1),x3=rnorm(10,0,1),x4=rnorm(10,0,1))
#'
#' subdata <- getModelData(i,adj_matrix, variable_names,data)

getModelData <- function(i,adjacency_matrix, variable_names,data) {

  parents <- variable_names[adjacency_matrix[,i] == 1]
  if (length(parents) > 0) {
    data <- data[c(variable_names[i],parents)]
  } else {
    data <- data[variable_names[i]]
  }

  return(data)
}
