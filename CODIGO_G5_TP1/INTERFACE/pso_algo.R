source("otiutils.R")     # repair(), eval(), calc_upper()
library(pso)
set.seed(123)

# ---------- gera tabela diária com tudo o que o utilizador quer ver -------
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
    
    custo_stock <- sum(stock_prev * cost_unit)        # remover round(...)
    lucro_dia   <- rev - cW - cV - custo_stock        # descontar stock
    
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


optimize_pso <- function(pred_sales, weekend_vec,
                         max_iters = 100,
                         swarm_size = 50,
                         inertia = 0.5,
                         cognitive_c = 1.0,
                         social_c    = 2) {
  
  assign("pred_sales", pred_sales , envir = .GlobalEnv)
  assign("weekend"   , weekend_vec, envir = .GlobalEnv)
  
  D     <- length(calc_upper(pred_sales))
  lower <- rep(0, D)
  upper <- calc_upper(pred_sales)
  
  res <- psoptim(
    par     = rep(NA, D),
    fn      = function(s) -eval(repair(round(s))),  # maximizar lucro
    lower   = lower,  upper = upper,
    control = list(
      maxit       = max_iters,
      s           = swarm_size,
      w           = inertia,
      c.p         = cognitive_c,
      c.g         = social_c,
      trace       = 0
    )
  )
  
  sol   <- repair(round(res$par))
  lucro <- -res$value
  
  list(
    lucro    = lucro,
    sol      = sol,
    detalhes = .mk_details(sol, pred_sales, weekend_vec)
  )
}