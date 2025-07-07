# ga_algo1.R
library(GA)
library(mco)
source("otiutils.R")

# GA sem pesos, devolve detalhamento + lucro + recursos
optimize_ga1 <- function(pred_sales, weekend_vec,
                         pop_size   = 50,
                         max_iter   = 100,
                         pmutation  = 0.2) {
  
  K_W <- 0.5
  
  obj_fun <- function(x) {
    s2 <- repair(round(x))
    lucro <- eval(s2)
    total_recursos <- sum(s2[1:7]) +
      sum(matrix(s2[8:28], nrow = 7, byrow = TRUE))
    c(lucro, total_recursos)
  }
  
  ga_fit <- function(x) {
    res <- obj_fun(x)
    -(-K_W * 0.5 * res[1] + (1 - 0.5) * res[2])
  }
  
  dim_s <- length(calc_upper(pred_sales))
  lower <- rep(0, dim_s)
  upper <- calc_upper(pred_sales)
  
  GA_res <- ga(
    type     = "real-valued",
    fitness  = ga_fit,
    lower    = lower,
    upper    = upper,
    popSize  = pop_size,
    maxiter  = max_iter,
    pmutation= pmutation
  )
  
  sol <- GA_res@solution[1, ]
  obj <- obj_fun(sol)
  lucro <- obj[1]
  recursos <- obj[2]
  
  # Uso da função auxiliar para calcular os detalhes completos
  detalhes <- calculaDetalhes(sol, pred_sales, weekend_vec)
  
  list(
    detalhes = detalhes,
    lucro    = lucro,
    recursos = recursos
  )
}


calculaDetalhes <- function(s2, pred_sales, weekend_vec) {
  # Separa os componentes da solução
  W     <- round(s2[1:7])
  Veic1 <- round(s2[8:14])
  Veic2 <- round(s2[15:21])
  Veic3 <- round(s2[22:28])
  
  # --- Cálculo da distribuição ---
  # Aqui você deve implementar a lógica real de distribuição.
  # No exemplo abaixo, usaremos um placeholder simples para distribuí-la.
  Dist <- matrix(NA, nrow = 7, ncol = 6)
  colnames(Dist) <- paste0("Dist", 1:6)
  for (i in 1:7) {
    # Exemplo: distribuição proporcional ao valor do armazém (apenas para exemplificação)
    total <- W[i] + 1  # evita divisão por zero
    Dist[i, ] <- round(rep(W[i] / (6*total), 6) * W[i])
  }
  
  # --- Cálculo dos custos ---
  # Se houver funções ou fatores específicos, inclua-os.
  # Exemplo: custo do armazém igual ao valor em W e custo do veículo a soma dos três veículos.
  CustoW <- W  
  CustoV <- Veic1 + Veic2 + Veic3
  
  # --- Cálculo dos indicadores diários ---
  # Aqui você deve aplicar a lógica de negócio para obter as vendas, as receitas etc.
  # Usaremos valores de exemplo, mas esses devem refletir os dados reais.
  VendasPot   <- rep(NA, 7)
  VendasReais <- rep(NA, 7)
  Receita     <- rep(NA, 7)
  LucroDia    <- rep(NA, 7)
  
  for (i in 1:7) {
    # Se você tiver dados de previsão (por exemplo, pred_sales), pode integrar aqui.
    # Usaremos exemplos fictícios:
    VendasPot[i]   <- 100 + i        # exemplo: vendas potenciais variando por dia
    VendasReais[i] <- 80 + i         # exemplo: vendas reais variando por dia
    Receita[i]     <- VendasReais[i] * 2  # supondo preço unitário de 2
    LucroDia[i]    <- Receita[i] - (CustoW[i] + CustoV[i])
  }
  
  # --- Construir o data frame completo ---
  data.frame(
    Dia         = 1:7,
    W           = W,
    Veic1       = Veic1,
    Veic2       = Veic2,
    Veic3       = Veic3,
    Dist1       = Dist[,1],
    Dist2       = Dist[,2],
    Dist3       = Dist[,3],
    Dist4       = Dist[,4],
    Dist5       = Dist[,5],
    Dist6       = Dist[,6],
    VendasPot   = VendasPot,
    VendasReais = VendasReais,
    CustoW      = CustoW,
    CustoV      = CustoV,
    CustoStock  = rep(NA, 7),  # se houver cálculos para custo de stock, insira a lógica aqui
    Receita     = Receita,
    LucroDia    = LucroDia
  )
}

