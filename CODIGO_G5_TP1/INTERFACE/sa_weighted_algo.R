library(GenSA)
library(dplyr)

optimize_sa2 <- function(pred_sales, 
                         weight_values = c(0.1, 0.3, 0.5, 0.7, 0.9), 
                         max_call = 500, 
                         seed = 123) {
  
  n_vars <- length(calc_upper(pred_sales))
  lower <- rep(0, n_vars)
  upper <- calc_upper(pred_sales)
  
  calc_cost <- function(s) {
    W <- s[1:7]
    V <- matrix(s[8:28], nrow = 7, ncol = 3, byrow = TRUE)
    weekend <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
    
    custo_W <- sum(W * ifelse(weekend, 15, 10))
    
    custo_V <- 0
    for (i in 1:7) {
      base_costs <- c(40, 50, 56)
      if (weekend[i]) base_costs <- base_costs + 10
      custo_V <- custo_V + sum(V[i, ] * base_costs)
    }
    
    custo_W + custo_V
  }
  
  results_list <- list()
  
  for (w1 in weight_values) {
    w2 <- 1 - w1
    
    objective_function <- function(s) {
      s <- pmin(pmax(round(s), lower), upper)
      s <- repair(s)
      
      lucro <- eval(s)
      
      if (lucro < 0) return(1e6)
      
      custo_total <- calc_cost(s)
      
      w1 * (-lucro) + w2 * custo_total
    }
    
    set.seed(seed)
    sa_result <- GenSA(
      par = rep(1, n_vars),
      lower = lower,
      upper = upper,
      fn = objective_function,
      control = list(max.call = max_call, verbose = FALSE)
    )
    
    best_solution <- round(sa_result$par)
    best_solution <- pmin(pmax(best_solution, lower), upper)
    best_solution_repaired <- repair(best_solution)
    
    best_profit <- eval(best_solution_repaired)
    best_cost <- calc_cost(best_solution_repaired)
    
    results_list[[paste0("w1_", w1)]] <- c(
      Lucro = best_profit,
      Recursos = best_cost,
      Alpha = w1
    )
  }
  
  pareto_mat <- do.call(rbind, results_list)
  pareto_df <- as.data.frame(pareto_mat)
  
  return(pareto_df)
}
