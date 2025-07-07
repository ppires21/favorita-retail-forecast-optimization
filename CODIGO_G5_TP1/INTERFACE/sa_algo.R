# sa_algo.R -----------------------------------------------------------------
# Simulated Annealing wrapper for interface
source("otiutils.R")       # for repair(), eval(), calc_upper()
library(GenSA)
set.seed(12345)

# ---------- Gera tabela diária com tudo o que o utilizador quer ver -------
.mk_details <- function(sol, pred_sales, weekend_vec) {
  W         <- sol[1:7]
  V         <- matrix(sol[8:28], nrow=7, ncol=3, byrow=TRUE)
  D         <- matrix(sol[29:70], nrow=7, ncol=6, byrow=TRUE)
  price     <- c(1.0,1.0,0.5,0.5,0.2,0.2)
  cost_unit <- c(0.1,0.1,0.05,0.05,0.05,0.05)
  base_veh  <- c(40,50,56)
  
  stock_prev <- rep(0,6)
  det        <- data.frame()
  
  for(i in 1:7) {
    avail        <- stock_prev + D[i,]
    real_sales   <- pmin(avail, pred_sales[i,])
    stock_prev   <- avail - real_sales
    
    cW           <- W[i] * ifelse(weekend_vec[i], 15, 10)
    cV           <- sum(V[i,] * (base_veh + ifelse(weekend_vec[i],10,0)))
    rev          <- sum(real_sales * price)
    lucro_dia    <- rev - cW - cV
    custo_stock  <- sum(stock_prev * cost_unit)
    
    det <- rbind(det, data.frame(
      Dia         = i,
      W           = W[i],
      Veic1       = V[i,1], Veic2 = V[i,2], Veic3 = V[i,3],
      Dist1       = D[i,1], Dist2 = D[i,2], Dist3 = D[i,3],
      Dist4       = D[i,4], Dist5 = D[i,5], Dist6 = D[i,6],
      VendasPot   = sum(pred_sales[i,]),
      VendasReais = sum(real_sales),
      CustoW      = cW,
      CustoV      = cV,
      CustoStock  = custo_stock,
      Receita     = rev,
      LucroDia    = lucro_dia
    ))
  }
  
  det
}

# ---------- Função chamada pela interface ---------------------------------
optimize_sa <- function(pred_sales, weekend_vec, max.call = 5000) {
  
  assign("pred_sales", pred_sales , envir = .GlobalEnv)
  assign("weekend"   , weekend_vec, envir = .GlobalEnv)
  
  upper <- calc_upper(pred_sales)
  lower <- rep(0, length(upper))
  
  fn_sa <- function(s) {
    s <- pmin(pmax(round(s), lower), upper)
    s <- repair(s)
    profit <- eval(s)
    if (profit < 0) return(1e6)
    return(-profit)
  }
  
  res <- GenSA(
    par     = rep(1, length(upper)),
    lower   = lower,
    upper   = upper,
    fn      = fn_sa,
    control = list(max.call = max.call, verbose = FALSE)
  )
  
  best_mem <- round(res$par)
  best_mem <- pmin(pmax(best_mem, lower), upper)
  best_rep <- repair(best_mem)
  lucro    <- eval(best_rep)
  
  list(
    lucro    = lucro,
    sol      = best_rep,
    detalhes = .mk_details(best_rep, pred_sales, weekend_vec)
  )
}
