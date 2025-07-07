# O1_F2_DE.R

# 0) Pacotes e fontes
library(rminer)
library(forecast)
library(DEoptim)

# funções auxiliares: repair(), eval(), calc_upper()
source("otiutils.R")

# 1) Função para otimização DE numa única semana (DEoptim)
enoptim_semana_DE <- function(pred_sales, weekend_vec, NP = 50L, itermax = 100L) {
  # disponibiliza no global para repair()/eval()
  assign("pred_sales", pred_sales, envir = .GlobalEnv)
  assign("weekend",    weekend_vec, envir = .GlobalEnv)
  
  # limites
  upper <- calc_upper(pred_sales)
  D     <- length(upper)
  lower <- rep(0, D)
  
  # controlo do DE
  ctrl <- DEoptim.control(
    NP           = NP,
    itermax      = itermax,
    F            = 0.8,
    CR           = 0.9,
    trace        = FALSE,
    storepopfrom = 1
  )
  
  # função-objetivo (maximizar lucro)
  res <- DEoptim(
    fn      = function(sol) -eval(repair(round(sol))),
    lower   = lower,
    upper   = upper,
    control = ctrl
  )
  
  best_mem  <- round(res$optim$bestmem)
  lucro     <- -res$optim$bestval
  hist_best <- -res$member$bestvalit
  
  list(
    lucro       = lucro,
    sol         = best_mem,
    profit_hist = hist_best
  )
}

# 2) Gera tabela de detalhes diários a partir da solução
.mk_details_DE <- function(sol, pred_sales, weekend_vec) {
  # sol: vetor de comprimento 7 + 21 + 42 = 70
  W <- sol[1:7]
  V <- matrix(sol[8:28], nrow = 7, ncol = 3, byrow = TRUE)
  D <- matrix(sol[29:70], nrow = 7, ncol = 6, byrow = TRUE)
  
  price     <- c(1.0,1.0,0.5,0.5,0.2,0.2)
  cost_unit <- c(0.1,0.1,0.05,0.05,0.05,0.05)
  base_veh  <- c(40,50,56)
  
  stock_prev <- rep(0, 6)
  det <- data.frame()
  
  for (i in 1:7) {
    avail      <- stock_prev + D[i, ]
    real_sales <- pmin(avail, pred_sales[i, ])
    stock_prev <- avail - real_sales
    
    cW      <- W[i] * ifelse(weekend_vec[i], 15, 10)
    cV      <- sum(V[i, ] * (base_veh + ifelse(weekend_vec[i], 10, 0)))
    rev     <- sum(real_sales * price)
    custo_stock <- sum(stock_prev * cost_unit)
    lucro_dia   <- rev - cW - cV - custo_stock
    
    det <- rbind(det, data.frame(
      Dia         = i,
      W           = W[i],
      Veic1       = V[i,1], Veic2 = V[i,2], Veic3 = V[i,3],
      Dist1       = D[i,1], Dist2 = D[i,2], Dist3 = D[i,3],
      Dist4       = D[i,4], Dist5 = D[i,5], Dist6 = D[i,6],
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

# 3) Função para Shiny: executa DE numa única semana e devolve detalhes
optimize_de <- function(pred_sales, weekend_vec, NP = 50L, itermax = 100L, aggr = "median") {
  # parâmetro aggr incluído apenas para compatibilidade
  out <- enoptim_semana_DE(pred_sales, weekend_vec, NP = NP, itermax = itermax)
  detalhes <- .mk_details_DE(out$sol, pred_sales, weekend_vec)
  recursos_totais <- sum(detalhes$W) + sum(detalhes$Veic1 + detalhes$Veic2 + detalhes$Veic3)
  list(
    lucro       = out$lucro,
    recursos    = recursos_totais,
    sol         = out$sol,
    detalhes    = detalhes,
    profit_hist = out$profit_hist
  )
}

# --- FIM: sem execução automática ao carregar este ficheiro ---
