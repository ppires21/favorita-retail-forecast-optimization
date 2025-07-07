library(mco)
source("otiutils.R")

optimize_hill <- function(pred_sales, weekend_vec, wts = c(0.1, 0.25, 0.5, 0.75, 0.9), max_iter = 1000) {
  K_W <- 0.5
  
  obj_fun <- function(x) {
    s2 <- repair(round(x))
    lucro <- eval(s2)
    total_recursos <- sum(s2[1:7]) + sum(matrix(s2[8:28], nrow = 7, byrow = TRUE))
    return(c(lucro, total_recursos))  # Mantemos o lucro positivo
  }
  
  hill_wfun <- function(x, W) {
    res <- obj_fun(x)
    return(-K_W * W * res[1] + (1 - W) * res[2])  # Negamos lucro para manter score a minimizar
  }
  
  dim_s <- length(calc_upper(pred_sales))
  lower <- rep(0, dim_s)
  upper <- calc_upper(pred_sales)
  
  hill_climbing_simple <- function(W) {
    best <- runif(dim_s, min = lower, max = upper)
    best_score <- hill_wfun(best, W)
    best_obj <- obj_fun(best)
    
    for (i in 1:max_iter) {
      candidate <- best + rnorm(dim_s, mean = 0, sd = 1)
      candidate <- pmin(pmax(candidate, lower), upper)
      score <- hill_wfun(candidate, W)
      if (score < best_score) {
        best <- candidate
        best_score <- score
        best_obj <- obj_fun(candidate)
      }
    }
    
    return(best_obj)
  }
  
  results <- do.call(rbind, lapply(wts, hill_climbing_simple))
  colnames(results) <- c("lucro_neg", "recursos")
  results_df <- data.frame(
    Lucro = -results[, "lucro_neg"],
    Recursos = results[, "recursos"],
    Alpha = wts
  )
  
  return(results_df)
}
