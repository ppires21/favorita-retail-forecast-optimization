# ----------------------------------------
# Hill Climbing
# ----------------------------------------

library(rminer)
library(forecast)

source("otiutils.R")  # funções: repair(), eval(), calc_upper()
source("hill.R")      # função: hclimbing()

set.seed(12345)

# ---------- Geração da tabela detalhada por dia (semelhante à PSO) ----------
.mk_details <- function(sol, pred_sales, weekend_vec) {
  W         <- sol[1:7]
  V         <- matrix(sol[8:28], nrow = 7, ncol = 3, byrow = TRUE)
  D         <- matrix(sol[29:70], nrow = 7, ncol = 6, byrow = TRUE)
  price     <- c(1.0, 1.0, 0.5, 0.5, 0.2, 0.2)
  cost_unit <- c(0.1, 0.1, 0.05, 0.05, 0.05, 0.05)
  base_veh  <- c(40, 50, 56)
  
  stock_prev <- rep(0, 6)
  det <- data.frame()
  
  for (i in 1:7) {
    avail      <- stock_prev + D[i, ]
    real_sales <- pmin(avail, pred_sales[i, ])
    stock_prev <- avail - real_sales
    
    cW         <- W[i] * ifelse(weekend_vec[i], 15, 10)
    cV         <- sum(V[i, ] * (base_veh + ifelse(weekend_vec[i], 10, 0)))
    rev        <- sum(real_sales * price)
    custo_stock <- sum(stock_prev * cost_unit)
    lucro_dia   <- rev - cW - cV - custo_stock
    
    det <- rbind(det, data.frame(
      Dia         = i,
      W           = W[i],
      Veic1       = V[i, 1], Veic2 = V[i, 2], Veic3 = V[i, 3],
      Dist1       = D[i, 1], Dist2 = D[i, 2], Dist3 = D[i, 3],
      Dist4       = D[i, 4], Dist5 = D[i, 5], Dist6 = D[i, 6],
      VendasPot   = sum(pred_sales[i, ]),
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

# ---------- Função de avaliação monitorada ----------
m_eval <- function(s) {
  s_reparado <- repair(s)
  res <- eval(s_reparado)
  
  EV <<- EV + 1
  BEST <<- max(BEST, res)
  if (EV <= MAXIT) F[EV] <<- BEST
  
  return(res)
}

# ---------- Mutação customizada ----------
rchange_custom <- function(par, lower, upper) {
  par_new <- par
  delta <- rnorm(42, mean = 0, sd = 10)
  par_new[29:70] <- par_new[29:70] + delta
  return(par_new)
}

# ---------- Otimizador principal ----------
optimize_hillClimbing <- function(pred_sales, weekend_vec,
                          max_iters = 500) {
  
  assign("pred_sales", pred_sales , envir = .GlobalEnv)
  assign("weekend"   , weekend_vec, envir = .GlobalEnv)
  
  # Reset de variáveis globais
  assign("EV", 0, envir = .GlobalEnv)
  assign("BEST", -Inf, envir = .GlobalEnv)
  assign("MAXIT", max_iters, envir = .GlobalEnv)
  assign("F", rep(NA, max_iters), envir = .GlobalEnv)
  
  D     <- length(calc_upper(pred_sales))
  lower <- rep(0, D)
  upper <- calc_upper(pred_sales)
  
  initial_par <- rep(0, D)
  initial_par[29:70] <- as.vector(t(pred_sales))
  initial_par <- repair(initial_par)
  
  res <- hclimbing(
    par     = initial_par,
    fn      = function(s) m_eval(s),
    change  = rchange_custom,
    lower   = lower,
    upper   = upper,
    control = list(maxit = max_iters, REPORT = 100, digits = 0),
    type    = "max"
  )
  
  sol <- round(res$sol)
  lucro <- res$eval
  
  list(
    lucro      = lucro,
    sol        = sol,
    detalhes   = .mk_details(sol, pred_sales, weekend_vec),
    historico  = F
  )
}
