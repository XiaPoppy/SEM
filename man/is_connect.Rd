% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/separate_graph.R
\name{is_connect}
\alias{is_connect}
\title{Separate Unconnected Graph}
\usage{
is_connect(edgelist, directed = TRUE)
}
\arguments{
\item{edgelist}{a list describing the edges from a graph.}

\item{directed}{true or false.}

\item{node_name}{the name of the variable.}
}
\value{
an edge-list from a subgraph containing the given node i(if possible).
}
\description{
Want to know whether a graph is connected or not. If unconnected, I want to be able
to get a subgraph which contains a certain node we give.
}
\examples{
#

}
