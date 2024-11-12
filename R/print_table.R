#' Print Dvine based SEM in a table
#'
#'
#' @param vc a list of vinereg object.
#' @return a table describing vine type between variables.
#' @export
#' @examples
#' #
#'
print_mod <- function(vc){
  vine_df <- data.frame()

  for (i in 1:length(vc)) {
    if(class(vc[[i]])=='vinereg'){
      summary_stats <- summary(vc[[i]]$vine,digits=2)
      #rewrite conditioning and conditioned columns:
      for(j in 1:length(rownames(summary_stats))){
        summary_stats$conditioned[[j]] <-
          vc[[i]]$vine$names[summary_stats$conditioned[[j]]]
        summary_stats$conditioning[[j]] <-
          vc[[i]]$vine$names[summary_stats$conditioning[[j]]]
      }
      # Append the summary statistics
      vine_df <- rbind(vine_df, as.data.frame(summary_stats))
    }

  }

  vine_df$conditioned <- sapply(vine_df$conditioned, toString)
  vine_df$conditioning <- sapply(vine_df$conditioning, toString)
  vine_df$parameters <- sapply(vine_df$parameters, toString)

  numeric_cols <- sapply(vine_df, is.numeric)

  # Rounding the numeric columns to two digits
  vine_df[, numeric_cols] <- round(vine_df[, numeric_cols], 2)

  # Printing the updated dataframe
  print(vine_df[c(3,4,6,7,8,9,10,11)])
  #xtable(vine_df[c(3,4,5,6,7,9,10,11)])#latex output
}


#'Print Dvine based SEM in a table
#'
#'
#' @param vc a list of vinereg object.
#' @return a table of all statistic scores.
#' @export
print_stat <- function(vc){
  summary_df <- data.frame()
  for (i in 1:length(vc)) {

    if(class(vc[[i]])=='vinereg'){
      summary_stats <- summary(vc[[i]],digits=2)#$vine
      #summary_stats$var[[1]] <- vc[[i]]$formula

      for(j in 1:length(summary_stats$var)){

        summary_stats$var[[j]] <- paste(vc[[i]]$vine$names[1], '|',
                                        summary_stats$var[[j]])
        j <- j + 1
      }

      # Append the summary statistics to the data frame
      summary_df <- rbind(summary_df, as.data.frame(summary_stats))
    }

  }

  numeric_cols <- sapply(summary_df, is.numeric)
  # Rounding the numeric columns to two digits
  summary_df[, numeric_cols] <- round(summary_df[, numeric_cols], 2)
  # Printing the updated dataframe
  print(summary_df)
  #xtable(summary_df)#latex output
}
