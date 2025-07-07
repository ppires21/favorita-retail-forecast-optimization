library(GenSA)
library(ggplot2)
library(dplyr)

n_vars <- 70
lower <- rep(0, n_vars)
upper <- calc_upper(pred_sales)  # Assuming this function is defined

weight_values <- c(0.1, 0.3, 0.5, 0.7, 0.9)

# Helper function to calculate total cost from a solution vector s
calc_cost <- function(s) {
  W <- s[1:7]
  V <- matrix(s[8:28], nrow=7, ncol=3, byrow=TRUE)
  weekend <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  
  custo_W <- sum(W * ifelse(weekend, 15, 10))
  
  custo_V <- 0
  for (i in 1:7) {
    base_costs <- c(40, 50, 56)
    if (weekend[i]) base_costs <- base_costs + 10
    custo_V <- custo_V + sum(V[i, ] * base_costs)
  }
  
  custo_total <- custo_W + custo_V
  return(custo_total)
}

results_list <- list()

for (w1 in weight_values) {
  w2 <- 1 - w1
  
  objective_function <- function(s) {
    s <- pmin(pmax(round(s), lower), upper)
    s <- repair(s)
    
    lucro <- eval(s)
    
    W <- s[1:7]
    V <- matrix(s[8:28], nrow=7, ncol=3, byrow=TRUE)
    
    weekend <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
    
    custo_W <- sum(W * ifelse(weekend, 15, 10))
    
    custo_V <- 0
    for (i in 1:7) {
      base_costs <- c(40, 50, 56)
      if (weekend[i]) base_costs <- base_costs + 10
      custo_V <- custo_V + sum(V[i, ] * base_costs)
    }
    
    custo_total <- custo_W + custo_V
    
    if (lucro < 0) return(1e6)
    
    objetivo <- w1 * (-lucro) + w2 * custo_total
    return(objetivo)
  }
  
  set.seed(123)  # For reproducibility
  sa_result <- GenSA(
    par = rep(1, n_vars),
    lower = lower,
    upper = upper,
    fn = objective_function,
    control = list(max.call = 5000, verbose = FALSE)
  )
  
  best_solution <- round(sa_result$par)
  best_solution <- pmin(pmax(best_solution, lower), upper)
  best_solution_repaired <- repair(best_solution)
  
  best_profit <- eval(best_solution_repaired)
  best_cost <- calc_cost(best_solution_repaired)
  
  results_list[[paste0("w1_", w1)]] <- list(
    w1 = w1,
    w2 = w2,
    solution = best_solution_repaired,
    profit = best_profit,
    cost = best_cost,
    objective_value = sa_result$value
  )
  
  cat("Weights (profit, cost):", w1, ",", w2, "\n")
  cat(">> Melhor lucro encontrado:", best_profit, "EUR\n")
  cat(">> Custo total:", best_cost, "EUR\n\n")
}

# Convert results_list to data.frame for plotting
results_df <- do.call(rbind, lapply(results_list, function(res) {
  data.frame(
    w1 = res$w1,
    w2 = res$w2,
    profit = res$profit,
    cost = res$cost
  )
}))

# Sort results by increasing cost
results_df <- results_df %>% arrange(cost)

# Select points that form the Pareto line: strictly increasing profit with increasing cost
pareto_line_points <- results_df[1, , drop=FALSE]  # start with first point

for (i in 2:nrow(results_df)) {
  if (results_df$profit[i] > pareto_line_points$profit[nrow(pareto_line_points)]) {
    pareto_line_points <- rbind(pareto_line_points, results_df[i, ])
  }
}

# Plot: all points with blue dots; pareto front line in red (connecting only non-dominated points)
ggplot() +
  geom_point(data = results_df, aes(x = cost, y = profit), color = "blue", size = 3) +
  geom_line(data = pareto_line_points, aes(x = cost, y = profit), color = "red", size = 1) +
  labs(
    title = "Pareto Curve: Profit vs Cost",
    x = "Total Cost (EUR)",
    y = "Profit (EUR)"
  ) +
  theme_minimal()
