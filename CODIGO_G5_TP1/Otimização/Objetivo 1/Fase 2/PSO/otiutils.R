
# Função repair (sem outros argumentos além de s)
repair <- function(s) {
  s <- round(s)
  W <- s[1:7]
  V <- matrix(s[8:28], nrow=7, ncol=3, byrow=TRUE)
  D <- matrix(s[29:70], nrow=7, ncol=6, byrow=TRUE)
  
  Q <- rowSums(D)
  
  # Armazém: 220 unidades por recurso
  need_W <- ceiling(Q / 220)
  W <- pmax(W, need_W)
  
  # Veículos: capacidades 200, 300, 390
  cap <- c(200, 300, 390)
  V_new <- matrix(0, nrow=7, ncol=3)
  for (i in 1:7) {
    rem <- Q[i]
    n3 <- floor(rem / cap[3]); rem <- rem - n3*cap[3]
    n2 <- floor(rem / cap[2]); rem <- rem - n2*cap[2]
    n1 <- ceiling(max(rem, 0) / cap[1])
    V_new[i, ] <- c(n1, n2, n3)
  }
  V <- V_new
  
  s2 <- c(W, as.vector(t(V)), as.vector(t(D)))
  return(round(s2))
}

# Função de avaliação corrigida completamente
eval <- function(s) {
  s <- round(s)
  
  W <- s[1:7]
  V <- matrix(s[8:28], nrow=7, ncol=3, byrow=TRUE,
              dimnames = list(NULL, c("v1","v2","v3")))
  D <- matrix(s[29:70], nrow=7, ncol=6, byrow=TRUE,
              dimnames = list(NULL, c("wd1","wd2","we1","we2","wb1","wb2")))
  
  if (!exists("weekend", envir = .GlobalEnv))
  stop("Tem de definir ‘weekend’ antes de chamar eval().")
  weekend <- get("weekend", envir = .GlobalEnv)
  
  # Capacidades
  Q <- rowSums(D)
  ware_cap <- W * 220
  cap_veh <- V[,1]*200 + V[,2]*300 + V[,3]*390
  
  # Mostrar previsões (como no slide)
  cat("> Forecasted sales:\n")
  print(pred_sales)
  
  cat("> Distributed products per day:\n")
  print(D)
  
  # 1) Armazém - custo depende do dia da semana
  cat("> Warehousing resources:", paste(W, collapse = " "), "\n")
  cost_W <- W * ifelse(weekend, 15, 10)  # 10 EUR dia útil, 15 EUR fim de semana
  cat("> total Warehousing resources cost=", sum(cost_W), "\n")
  cat("check if Warehousing resources can distribute all products:\n")
  print(Q <= ware_cap)
  
  # 2) Veículos - custo base + 10 EUR extra no fim de semana
  cat(">Vehicle resources per day:\n")
  print(V)
  
  # Custo base dos veículos
  cost_V <- matrix(0, nrow=7, ncol=3)
  for(i in 1:7) {
    # Custo base de cada tipo de veículo
    base_costs <- c(40, 50, 56)
    
    # Se é fim de semana, adiciona 10 EUR por veículo
    if(weekend[i]) {
      base_costs <- base_costs + 10
    }
    
    # Calcula custo total para cada tipo de veículo neste dia
    cost_V[i,] <- V[i,] * base_costs
  }
  
  cat("> vehicle total cost=", sum(cost_V), "\n")
  cat("check if vehicles can distribute all products:\n")
  print(Q <= cap_veh)
  
  # 3) Simulação de vendas e stock
  stock <- matrix(0, nrow=8, ncol=6)  # Começa com stock=0 e terá 8 linhas (inclui o inicial)
  sold  <- matrix(0, nrow=7, ncol=6)  # Vendas para 7 dias
  
  for (i in 1:7) {
    for (j in 1:6) {
      # O que está disponível = stock atual + distribuído
      avail <- stock[i,j] + D[i,j]
      forecast_sales <- pred_sales[i,j]
      
      if (forecast_sales > avail) {
          sold[i,j]    <- avail
          stock[i+1,j] <- 0     # repor a 0, tal como no exemplo do professor
        } else {
          sold[i,j]    <- forecast_sales
          stock[i+1,j] <- avail - forecast_sales
        }
    }
  }
  
  cat("[1] \"True sales:\"\n")
  print(sold)
  
  # Preços e custos por família de produto
  price <- c(1.0, 1.0, 0.5, 0.5, 0.2, 0.2)  # dairy, dairy, eggs, eggs, bread, bread
  cost_unit <- c(0.1, 0.1, 0.05, 0.05, 0.05, 0.05)
  
  # Receita total das vendas
  revenue <- sum(colSums(sold) * price)
  cat("> total sales revenue=", revenue, "\n")
  
  cat("[1] \"Stock:\"\n")
  print(stock[2:8,])
  
  # Custo de armazenamento dos produtos não vendidos
  cost_stock <- sum(colSums(stock[2:8,]) * cost_unit)
  cat("> total stock cost=", cost_stock, "\n")
  
  # 4) Custos totais e lucro
  total_cost <- sum(cost_W) + sum(cost_V) + cost_stock
  cat("[1] \"Total costs:\"\n")
  cat("> total costs=", sum(cost_W), "+", sum(cost_V), "+", cost_stock, "=", total_cost, "\n")
  
  profit <- revenue - total_cost
  cat(">> Week profit=", revenue, "-", total_cost, "=", profit, "EUR\n")
  
  # Total de recursos
  total_resources <- sum(W) + sum(V)
  cat(">> Week resources=", sum(W), "+", sum(rowSums(V)), "=", total_resources, "\n")
  
  return(profit)
}


calc_upper <- function(pred_sales) {
  daily_totals <- rowSums(pred_sales)
  cum_totals   <- rev(cumsum(rev(daily_totals)))
  # Armazéns
  upper_W2 <- ceiling(cum_totals / 220)
  # Veículos (7 dias × 3 tipos) usando outer para vectorização
  caps        <- c(200, 300, 390)
  upper_V2_mat <- ceiling(outer(cum_totals, 1 / caps))
  upper_V2    <- as.vector(t(upper_V2_mat))
  # Distribuição (7 dias × 6 produtos) também vetorizado
  upper_D2_mat <- t(sapply(1:ncol(pred_sales),
                           function(j) rev(cumsum(rev(pred_sales[, j])))))
  upper_D2    <- as.vector(t(upper_D2_mat))
  c(upper_W2, upper_V2, upper_D2)
}
