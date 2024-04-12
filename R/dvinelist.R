#' Get List of Dvine Objects and Quantile plots
#'
#'
#'
#' @param fomula a list of model fomulas.
#' @param adjacency_matrix the adjacency matrix.
#' @param variable_names vector of variable names.
#' @param data x-scaled data, used to train the dvine model.
#' @param uscale default value: FALSE, used in \code{vinereg::vinereg}.
#' @param family_set used in \code{vinereg::vinereg}.
#' @param u_data optional parameter, used when uscale=TRUE
#' @return a list of dvine objects and a list of quantile graphs
#' @export
#' @examples
#' #
#'
source(get_model_data.R)
dvinelist <- function(fomula,adjacency_matrix,variable_names,data,
                      uscale=FALSE,family_set = 'parametric',u_data=NULL){

  vc <- vector('list',length=length(variable_names))
  leng <- 1:length(variable_names)
  l_vc <- 0 #storing number of nodes who has parents

  #j <- 1
  for (i in leng) {
    parents <- variable_names[adjacency_matrix[,i] == 1]
    if (length(parents) > 0) {
      if(uscale){
        x <- getModelData(i,adjacency_matrix, variable_names,u_data)
        vc[[i]] <- vinereg(fomula1[[i]], x,family_set = family_set,uscale = TRUE)
        l_vc <- l_vc + 1
      }else{
        x <- getModelData(i,adjacency_matrix, variable_names,data)
        vc[[i]] <- vinereg(fomula1[[i]], x,family_set = family_set,uscale = FALSE)
        l_vc <- l_vc + 1
      }
      #j <- j+1
    } else {vc[[i]] <- u.data[[names(x)]]}
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

  return(vc,fig)
}
