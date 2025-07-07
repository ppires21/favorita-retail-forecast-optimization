# ga_algo2.R
library(GA)
library(mco)
source("otiutils.R")

# GA “com pesos” que devolve a frente de Pareto (Lucro vs Recursos)
optimize_ga2 <- function(pred_sales, weekend_vec,
                         wts       = c(0.1, 0.25, 0.5, 0.75, 0.9),
                         pop_size  = 50,
                         max_iter  = 10,
                         pmutation = 0.2) {
  
  K_W <- 0.5
  
  obj_fun <- function(x) {
    s2 <- repair(round(x))
    lucro <- eval(s2)
    total_recursos <-
      sum(s2[1:7]) +
      sum(matrix(s2[8:28], nrow = 7, byrow = TRUE))
    c(lucro, total_recursos)
  }
  
  # score a minimizar
  ga_wfun <- function(x, W) {
    res <- obj_fun(x)
    -K_W * W * res[1] + (1 - W) * res[2]
  }
  
  dim_s <- length(calc_upper(pred_sales))
  lower <- rep(0, dim_s)
  upper <- calc_upper(pred_sales)
  
  run_ga_W <- function(W) {
    res <- ga(
      type     = "real-valued",
      fitness  = function(x) -ga_wfun(x, W),
      lower    = lower,
      upper    = upper,
      popSize  = pop_size,
      maxiter  = max_iter,
      pmutation= pmutation
    )
    sol <- res@solution[1, ]
    obj <- obj_fun(sol)
    c(Lucro = obj[1], Recursos = obj[2], Alpha = W)
  }
  
  pareto_mat <- t(sapply(wts, run_ga_W))
  pareto_df  <- as.data.frame(pareto_mat)
  pareto_df
}
