#' Get List of Dvine Objects
#'
#'
#'
#' @param fomula a list of model fomulas.
#' @param adjacency_matrix the adjacency matrix.
#' @param variable_names vector of variable names.
#' @param data x-scaled data, used to train the dvine model.
#' @param uscale default value: FALSE, used in \code{vinereg::vinereg}.
#' @param family_set used in \code{vinereg::vinereg}.
#' @return a list of dvine objects and a list of quantile graphs
#' @export
#' @examples
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
#' fomula <- createModel(adjacency_matrix, variable_names,'list')
#' vc_list <- dvinelist(fomula, adjacency_matrix, variable_names,t1)
#'
#source('get_model_data.R')


dvinelist <- function(fomula,adjacency_matrix,variable_names,data,
                      uscale=FALSE,family_set = 'parametric'){

  vc <- vector('list',length=length(variable_names))
  leng <- 1:length(variable_names)
  l_vc <- 0 #storing number of nodes who has parents

  #j <- 1
  for (i in leng) {
    parents <- variable_names[adjacency_matrix[,i] == 1]
    if (length(parents) > 0) {
      if(uscale){
        x <- getModelData(i,adjacency_matrix, variable_names,u_data)
        vc[[i]] <- vinereg(fomula[[i]], x,family_set = family_set,uscale = TRUE)
        l_vc <- l_vc + 1
      }else{
        x <- getModelData(i,adjacency_matrix, variable_names,data)
        vc[[i]] <- vinereg(fomula[[i]], x,family_set = family_set,uscale = FALSE)
        l_vc <- l_vc + 1
      }
      #j <- j+1
    } else {vc[[i]] <- 0}
  }

  fig <- vector('list',length=l_vc)
  j <- 1
  for ( i in leng){
    parents <- variable_names[adjacency_matrix[,i] == 1]
    if(length(parents)>0){
      fig[[j]]<-plot_effects(vc[[i]])
      grid.arrange(fig[[j]], ncol = 1)
      grid.text(vc[[i]][['formula']], x = 0.5, y = 0.99, just = "center",
                gp = gpar(fontsize = 8, fontface = "bold"))
      j <- j+1
    }
  }

  return(vc)
}

#' @export
getModelData <- function(i,adjacency_matrix, variable_names,data) {

  parents <- variable_names[adjacency_matrix[,i] == 1]
  if (length(parents) > 0) {
    data <- data[c(variable_names[i],parents)]
  } else {
    data <- data[variable_names[i]]
  }

  return(data)
}
