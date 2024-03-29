% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_model.R
\name{createModelList}
\alias{createModelList}
\title{Create Model Fomulas}
\usage{
createModelList(adjacency_matrix, variable_names)
}
\arguments{
\item{adjacency_matrix}{the adjacency matrix.}

\item{variable_names}{a vector of variable names.}
}
\value{
a list/string that stores the relation fomula of each node.
}
\description{
Return an object which stores the dependence fomula, e.g. 'y ~ x1 + x2' of
each node with two different data types.
}
\examples{
adj_mat <- matrix(c(0,0,0,1,0,0,0,1,0), nrow=3, ncol=3, byrow=TRUE)
variable_names <- c('x1','x2','x3')

# get a list object
createModelList(adj_matrix, variable_names)

# get a string object
createModelString(adj_matrix, variable_names)
}
