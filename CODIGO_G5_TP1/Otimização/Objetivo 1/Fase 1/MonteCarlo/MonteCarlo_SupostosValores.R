# 1) Carregar dados e extrair previsões e datas
sales <- read.csv("sales.csv", sep=";")
sales$date <- as.Date(sales$date)

# Últimos 7 dias (horizonte de 1 semana)
last_7 <- tail(sales, 7)

# Matriz de previsões: colunas na ordem 
# d11, d12, e11, e12, b11, b12
#pred <- as.matrix(last_7[, c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")])
pred <- matrix(c(
  626, 366, 177,  38, 472, 181,
  575, 341, 340,  92, 868, 175,
  608, 387, 163,  65, 634, 145,
  527, 294, 170,  43, 582, 139,
  569, 381, 338,  45, 582, 184,
  603, 411, 275, 117, 583, 275,
  611, 373, 438,  15,2080, 176
), nrow = 7, byrow = TRUE)
cat("> Forecasted sales (pred):\n")
print(pred)

# Datas correspondentes, para detetar fins de semana
dates <- last_7$date

# 2) Função de reparação: garante que há recursos suficientes
repair <- function(s, pred, dates) {
  s <- round(s)  # para garantir inteiros
  # separar componentes
  W <- s[1:7]                         # armazém por dia
  V <- matrix(s[8:28], nrow=7, ncol=3, byrow=TRUE)   # numero de veiculos por tipo v1,v2,v3 por dia
  D <- matrix(s[29:70], nrow=7, ncol=6, byrow=TRUE)  # numero de produtos entregues por tipo por dia
  
  # total de produtos a distribuir em cada dia
  Q <- rowSums(D)
  
  # 2.1) Warehousing: cada recurso faz 220 produtos/dia
  need_W <- ceiling(Q / 220)
  W <- pmax(W, need_W)
  
  # 2.2) Veículos: capacidades e custo-eficiência
  cap <- c(200,300,390)
  V_new <- matrix(0, nrow=7, ncol=3)
  for (i in 1:7) {
    rem <- Q[i]
    # alocar por ordem decrescente de capacidade (greedy)
    n3 <- floor(rem / cap[3]); rem <- rem - n3*cap[3]
    n2 <- floor(rem / cap[2]); rem <- rem - n2*cap[2]
    n1 <- ceiling(max(rem,0) / cap[1])
    V_new[i, ] <- c(n1, n2, n3)
  }
  V <- V_new
  
  # reconstruir vetor
  s2 <- c(
    W,
    as.vector(t(V)),
    as.vector(t(D))
  )
  round(s2)
}

eval_sched_verbose <- function(s, pred, dates,
                               method = c("repair","penalty"),
                               penalty_value = -1e6) {
  method <- match.arg(method)
  s0     <- s
  s      <- round(s)
  
  # detectar fim de semana
  wday    <- as.POSIXlt(dates)$wday
  is_wend <- wday %in% c(0,6)
  
  # separar W, V, D
  W <- s[1:7]
  V <- matrix(s[8:28],  nrow=7, ncol=3, byrow=TRUE,
              dimnames = list(NULL, c("v1","v2","v3")))
  D <- matrix(s[29:70], nrow=7, ncol=6, byrow=TRUE,
              dimnames = list(NULL,
                              c("wd1","wd2","we1","we2","wb1","wb2")))
  Q        <- rowSums(D)
  ware_cap <- W * 220
  cap_veh  <- V[,1]*200 + V[,2]*300 + V[,3]*390
  
  # 1) Warehousing resources per day e custo total
  ware_cap <- W * 220
  cost_ware_day <- W * ifelse(is_wend, 15, 10)
  cat("> Warehousing resources per day:\n")
  print(data.frame(dia = 1:7, recursos = W, capacidade = ware_cap, custo = cost_ware_day))
  cat("> total Warehousing resources cost =", sum(cost_ware_day), "\n\n")
  cat("> check if warehousing resources can distribute all products:\n")
  print(Q <= ware_cap)
  
  # 2) Vehicle resources per day e custo total
  cap_v <- c(200,300,390)
  cost_v1 <- V[,"v1"] * (40 + 10*is_wend)
  cost_v2 <- V[,"v2"] * (50 + 10*is_wend)
  cost_v3 <- V[,"v3"] * (56 + 10*is_wend)
  cost_veh_day <- cost_v1 + cost_v2 + cost_v3
  cat("> Vehicle resources per day:\n")
  print(V)
  cat("> vehicle total cost =", sum(cost_veh_day), "\n\n")
  cat("> check if vehicles can distribute all products:\n")
  print(Q <= cap_veh)
  
  # 3) Simular vendas e stock
  stock <- matrix(0, nrow=8, ncol=6)
  sold  <- matrix(0, nrow=7, ncol=6)
  for (i in 1:7) for (j in 1:6) {
    avail        <- stock[i,j] + D[i,j]
    sold[i,j]    <- min(avail, pred[i,j])
    stock[i+1,j] <- max(avail - pred[i,j], 0)
  }
  colnames(sold) <- colnames(D)
  colnames(stock) <- colnames(D)
  
  # <<<<<< AQUI VAI O DEBUG >>>>>>
  price <- c(1.0,1.0,0.5,0.5,0.2,0.2)
  cat("> Receita diária (7 dias):\n")
  daily_rev <- rowSums(sold * price)
  print(daily_rev)
  cat("Soma diária:", sum(daily_rev), " (deveria ser 3093.9)\n\n")
  
  cost_per_unit <- c(0.1,0.1,0.05,0.05,0.05,0.05)
  cat("> Custo stock diário (7 dias):\n")
  daily_stock <- rowSums(stock[2:8,] * cost_per_unit)
  print(daily_stock)
  cat("Soma diária:", sum(daily_stock), " (deveria ser 93.1)\n\n")
  # <<<<<< FIM DO DEBUG >>>>>>
  
  
  cat("> True sales (vendido):\n")
  print(sold)
  total_rev <- sum(sold * c(1.0,1.0,0.5,0.5,0.2,0.2))
  cat("> total sales revenue =", total_rev, "\n\n")
  
  cat("> Stock remanescente:\n")
  # só linhas 2:8 (stock após cada dia)
  print(stock[2:8, ])
  cost_stock <- sum(stock[2:8, ] * c(0.1,0.1,0.05,0.05,0.05,0.05))
  cat("> total stock cost =", cost_stock, "\n\n")
  
  # 4) Total costs e lucro
  total_costs <- sum(cost_ware_day) + sum(cost_veh_day) + cost_stock
  cat("> total costs =", 
      sum(cost_ware_day), "+", sum(cost_veh_day), "+", cost_stock, 
      "=", total_costs, "\n\n")
  
  profit <- total_rev - total_costs
  week_resources <- sum(W) + sum(V)
  cat(">> Week profit =", total_rev, "-", total_costs, "=", profit, "EUR\n")
  cat(">> Week resources =", sum(W), "+", sum(V), "=", week_resources, "\n")
  
  return(profit)
}

# 4) Teste usando o exemplo dos slides 18-19

# 4.1) Construir solução s1 exactamente como no exemplo:
W1 <- c(10,  4, 0, 7, 4, 4, 0)
V1 <- matrix(c(
  11,0,0,
  0,1,2,
  0,0,0,
  4,2,0,
  0,3,0,
  0,2,1,
  0,0,0
), nrow=7, byrow=TRUE)
D1 <- matrix(c(
  200,150,175, 50,990,500,
  100, 64,300, 20,155,101,
  0,  0,  0,  0,  0,  0,
  700,300,100, 35,200, 23,
  200,234,140, 78,100, 75,
  100, 50,  8,230,300, 23,
  0,  0,  0,  0,  0,  0
), nrow=7, byrow=TRUE)

s1 <- c(
  W1,
  as.vector(t(V1)),
  as.vector(t(D1))
)

# Avaliar
print(eval_sched_verbose(s1, pred, dates, method="repair"))