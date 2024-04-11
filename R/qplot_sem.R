#' Get Quantile Plots from a Gaussia SEM
#'
#' @param fitted_sem The fitted gaussian structural model, a lavaan object.
#' @param dat original dataset used to generate sem.
#' @param alpha default value is (0.1, 0.5, 0.9).
#' @param adjacency_matrix the adjacency matrix.
#' @param variable_names vector of variable names.
#' @param fomula the model fomula used for sem, should be a list.
#' @export
#' @examples
#' # example code
#'
#'

qplot_sem <- function(fitted_sem, dat, alpha = c(0.1, 0.5, 0.9), adjacency_matrix, variable_names,fomula) {
  temp <- coef(fitted_sem)  # all coefficients

  for (i in 1:length(variable_names)) {
    parents <- variable_names[adjacency_matrix[, i] == 1]
    df <- data.frame(alpha = numeric(), predict = numeric(), value = numeric(), var = character())

    if (length(parents) > 0) {
      n_i <- variable_names[i]
      mean_simu <- temp[paste(n_i, "~1", sep = "")]  # beta_0

      for (k in 1:length(parents)) {
        j <- parents[k]
        beta_ij <- temp[paste(n_i, '~', j, sep = '')]
        mean_simu <- mean_simu + beta_ij * as.numeric(dat[[j]])  # b0 + b_ij * xj
      }

      for (k in 1:length(parents)) {
        j <- parents[k]
        sigma <- temp[paste(n_i, "~~", n_i, sep = "")]
        q_ij <- data.frame('0.1' = qnorm(0.1, mean_simu, sigma),
                           '0.5' = qnorm(0.5, mean_simu, sigma),
                           '0.9' = qnorm(0.9, mean_simu, sigma))
        q_ij <- gather(q_ij, key = 'alpha', value = 'predict')
        q_ij <- cbind(q_ij, data.frame(value = as.numeric(dat[[j]]), var = j))
        df <- rbind(df, q_ij)
      }
    }

    # plot
    if (nrow(df) > 0) {
      print(
        ggplot(df, aes(value, predict, color = alpha)) +
          geom_point(alpha = 0.15) +
          geom_smooth(se = FALSE) +
          facet_wrap(~var, scale = "free_x") +
          ylab(expression(Q(y * "|" * x[1] * ",...," * x[p]))) +
          xlab(expression(x[k])) +
          theme(legend.position = "bottom")

      )
      grid.text(fomula[i], x = 0.5, y = 0.99, just = "center", gp = gpar(fontsize = 8, fontface = "bold"))
    }
  }
}

