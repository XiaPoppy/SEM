#' Create Model Fomulas
#'
#' Return an object which stores the dependence fomula, e.g. 'y ~ x1 + x2' of
#' each node with two different data types.
#'
#' @param adjacency_matrix the adjacency matrix.
#' @param variable_names a vector of variable names.
#' @return a list/string that stores the relation fomula of each node.
#' @export
#' @examples
#' adj_mat <- matrix(c(0,0,0,1,0,0,0,1,0), nrow=3, ncol=3, byrow=TRUE)
#' variable_names <- c('x1','x2','x3')
#'
#' # get a list object
#' createModelList(adj_matrix, variable_names)
#'
#' # get a string object
#' createModelString(adj_matrix, variable_names)

createModelList <- function(adjacency_matrix, variable_names) {

  model_list <- list()

  for (i in seq_along(variable_names)) {
    parents <- variable_names[adjacency_matrix[,i] == 1]
    model_string <- paste0(variable_names[i], " ~ ",
                           paste(parents, collapse = " + "))#
    model_list <- append(model_list, model_string)
  }
  return(model_list)
}

createModelString <- function(adjacency_matrix, variable_names) {
# Return: a String that stores the relation fomula of each node

  model_string <- ""

  for (i in seq_along(variable_names)) {
    parents <- variable_names[adjacency_matrix[,i] == 1]
    if (length(parents) > 0) {
      model_string <- paste0(model_string, "\n", variable_names[i], " ~ ",
                             paste(parents, collapse = " + "))
    } else {
      model_string <- paste0(model_string, "\n", variable_names[i], " ~ 1")
    }
  }

  return(model_string)
}
