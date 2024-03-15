createModelList <- function(adjacency_matrix, variable_names) {
# Return: a list that stores the relation fomula of each node

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


getModelData <- function(i,adjacency_matrix, variable_names,data) {
#Return: data of variables in the fomula of i-th node

# Parameter description:
# i: the integral number taking from 1 to length(vairable_names)
# data: the data containing variable_names, can also be the u-scaled data

  parents <- variable_names[adjacency_matrix[,i] == 1]
  if (length(parents) > 0) {
    data <- data[c(variable_names[i],parents)]
  } else {
    data <- data[variable_names[i]]
  }

  return(data)
}
