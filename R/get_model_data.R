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
