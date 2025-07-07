# ficheiro: GA_ITER_GROWING_COM_CONVERGENCIA.R

# 0) Fontes das funções de otimização e semente
source("C:/Users/joaoa/OneDrive/Documentos/Rstudio/Otimização/Objetivo1/otiutilsF2.R")    # repair(), eval(), calc_upper()
source("C:/Users/joaoa/OneDrive/Documentos/Rstudio/Otimização/Objetivo1/blind.R")       # fsearch(), dfsearch()
set.seed(12345)

# 1) Carregar previsões
df_pred <- read.csv("C:/Users/joaoa/OneDrive/Documentos/Rstudio/Otimização/Objetivo1/ITER_Sales_prev.csv")
sales_cols       <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")
Runs             <- length(unique(df_pred$Iteracao))
forecast_horizon <- max(df_pred$Dia)

# 2) Carregar histórico de vendas
sales <- read.csv("C:/Users/joaoa/OneDrive/Documentos/Rstudio/sales_clean.csv", sep=";")
sales$date      <- as.Date(sales$date)
sales_cleaned   <- sales[1:(nrow(sales)-3), ]
all_dates       <- tail(sales_cleaned$date, 365)

# 3) Parâmetros do growing window
increment <- 7
W         <- 365 - forecast_horizon - (Runs - 1) * increment

# 4) Reconstruir lista de previsões
pred_list <- vector("list", Runs)
for (b in seq_len(Runs)) {
  sub            <- df_pred[df_pred$Iteracao == b, sales_cols]
  pred_list[[b]] <- as.matrix(sub)
}

# 5) Função de otimização com algoritmo genético
optimiza_semana_convergencia_GA <- function(pred_matrix, N = 100, pop_size = 50, mutation_rate = 0.01) {
  assign("pred_sales", pred_matrix, envir = .GlobalEnv)
  lower <- rep(0, 70)
  upper <- calc_upper(pred_sales)
  
  F     <- rep(NA, N)
  BEST  <- -Inf
  EV    <- 0
  
  eval_silent <- function(s, pred_sales) {
    sink(tempfile())
    on.exit(sink())
    eval(s)
  }
  
  fitness <- function(sol) {
    sol  <- repair(round(sol))
    res  <- eval_silent(sol, pred_sales)
    EV   <<- EV + 1
    BEST <<- max(BEST, res)
    if (EV <= N) F[EV] <<- BEST
    return(res)
  }
  
  # População inicial
  population <- replicate(pop_size, runif(length(lower), lower, upper), simplify = FALSE)
  fitness_scores <- sapply(population, fitness)
  
  while (EV < N) {
    # Seleção (torneio de 2)
    parents <- replicate(pop_size, {
      i <- sample(seq_along(population), 2)
      if (fitness_scores[i[1]] > fitness_scores[i[2]]) population[[i[1]]] else population[[i[2]]]
    }, simplify = FALSE)
    
    # Crossover (uniforme)
    offspring <- lapply(seq(1, pop_size, by=2), function(i) {
      p1 <- parents[[i]]
      p2 <- parents[[i+1]]
      mask <- runif(length(p1)) > 0.5
      c1 <- ifelse(mask, p1, p2)
      c2 <- ifelse(mask, p2, p1)
      list(c1, c2)
    })
    population <- unlist(offspring, recursive = FALSE)
    
    # Mutação
    population <- lapply(population, function(ind) {
      mutate_mask <- runif(length(ind)) < mutation_rate
      ind[mutate_mask] <- runif(sum(mutate_mask), lower[mutate_mask], upper[mutate_mask])
      ind
    })
    
    fitness_scores <- sapply(population, fitness)
  }
  
  best_idx <- which.max(fitness_scores)
  list(
    lucro = fitness_scores[best_idx],
    sol   = round(population[[best_idx]]),
    F     = F
  )
}

# 6) Loop por iteração
run_phase2_convergencia_GA <- function(pred_list, N = 5000, aggr = c("mean", "median")) {
  aggr   <- match.arg(aggr)
  lucros <- numeric(length(pred_list))
  sols   <- vector("list", length(pred_list))
  F_list <- vector("list", length(pred_list))
  
  for (i in seq_along(pred_list)) {
    start_idx  <- W + (i-1) * increment + 1
    week_dates <- all_dates[start_idx:(start_idx + forecast_horizon - 1)]
    Sys.setlocale("LC_TIME", "pt_PT.UTF-8")
    weekend_vec <- weekdays(week_dates) %in% c("sábado", "domingo")
    assign("weekend", weekend_vec, envir = .GlobalEnv)
    
    cat(sprintf("Semana %2d/%d — ", i, length(pred_list)))
    out <- optimiza_semana_convergencia_GA(pred_list[[i]], N = N)
    cat(sprintf("Lucro = %.2f EUR\n", out$lucro))
    lucros[i] <- out$lucro
    sols[[i]] <- out$sol
    F_list[[i]] <- out$F
  }
  
  lucro_agr <- if (aggr == "mean") mean(lucros) else median(lucros)
  cat("============================================\n")
  cat(sprintf("%s dos lucros = %.2f EUR\n", toupper(aggr), lucro_agr))
  
  list(
    lucros    = lucros,
    lucro_agr = lucro_agr,
    sols      = sols,
    F_list    = F_list
  )
}

# 7) Executar otimização com GA
resultados <- run_phase2_convergencia_GA(pred_list, N = 5000, aggr = "median")

df_res <- data.frame(
  Metodo        = "GeneticAlgorithm",
  Semana        = seq_along(resultados$lucros),
  Lucro         = resultados$lucros,
  LucroAgregado = resultados$lucro_agr
)

# 8) Imprimir resultados e gráfico
cat("\n\n--- Tabela de Lucros por Semana ---\n")
print(df_res)
cat(sprintf("\nLucro agregado (mediana) = %.2f EUR\n\n", resultados$lucro_agr))

# Gráfico por semana
plot(df_res$Semana, df_res$Lucro,
     type  = "b",
     pch   = 16,
     xlab  = "Semana", ylab = "Lucro (EUR)",
     main  = "Lucro por Semana — Algoritmo Genético (Fase 2)")
abline(h = resultados$lucro_agr, lty = 2)
legend("topright",
       legend = sprintf("Mediana = %.2f", resultados$lucro_agr),
       lty    = 2, bty = "n")

# 9) Gráfico de convergência com múltiplas cores
matplot(
  do.call(cbind, resultados$F_list),
  type = "l", lty = 1, lwd = 1.5,
  xlab = "Número de Avaliações",
  ylab = "Melhor Lucro Até Agora (EUR)",
  main = "Convergência por Semana — Algoritmo Genético",
  col  = rainbow(length(resultados$F_list))
)
legend("bottomright", legend = paste("Semana", seq_along(resultados$F_list)),
       col = rainbow(length(resultados$F_list)),
       lty = 1, bty = "n", cex = 0.8)
