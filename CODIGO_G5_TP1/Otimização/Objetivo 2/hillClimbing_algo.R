# -----------------------------------------------------
# Hill Climbing com Pesos — 10 Semanas (Frente de Pareto)
# -----------------------------------------------------

library(mco)
set.seed(12345)
source("otiutils.R")  # funções: repair(), eval(), calc_upper()

# 1) Leitura das previsões previamente guardadas
forecast_df <- read.csv("previsoes_10_iteracoes.csv")
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")
forecast_horizon <- 7
Runs <- 10

# 2) Reorganiza o dataframe em lista de matrizes 7×6 (por semana)
pred_list <- vector("list", Runs)
for (i in 1:Runs) {
  cols_i <- paste0(sales_cols, "_", i)
  mat_i <- as.matrix(forecast_df[1:forecast_horizon, cols_i])
  colnames(mat_i) <- sales_cols
  pred_list[[i]] <- mat_i
}

# 3) Utilitários
col_sort_matrix <- function(M, column = 1) {
  if (is.matrix(M)) {
    I <- sort.int(M[, column], index.return = TRUE)
    M <- M[I$ix, ]
  }
  return(M)
}

paretoFilter <- function(M) {
  I <- rep(TRUE, nrow(M))
  for (i in 1:(nrow(M) - 1)) {
    for (j in (i + 1):nrow(M)) {
      if (all(M[i, ] <= M[j, ]) && any(M[i, ] < M[j, ])) I[j] <- FALSE
      if (all(M[j, ] <= M[i, ]) && any(M[j, ] < M[i, ])) I[i] <- FALSE
    }
  }
  res <- M[I, , drop = FALSE]
  if (is.null(dim(res))) res <- matrix(res, ncol = ncol(M))
  return(res)
}

# 4) Parâmetros globais
MAXIT <- 1000
WTS <- c(0.1, 0.25, 0.5, 0.75, 0.9)
K_W <- 0.5

# 5) Funções Objetivo
obj_fun <- function(x) {
  s2 <- repair(round(x))
  lucro <- eval(s2)
  total_recursos <- sum(s2[1:7]) + sum(matrix(s2[8:28], nrow = 7, byrow = TRUE))
  return(c(lucro, total_recursos))
}

hill_wfun <- function(x, W) {
  res <- obj_fun(x)
  return(K_W * W * res[1] + (1 - W) * res[2])
}

# 6) Hill Climbing
hill_climbing_simple <- function(W, lower, upper, dim_s, max_iter = 1000, sigma = 1) {
  best <- runif(dim_s, min = lower, max = upper)
  best_score <- hill_wfun(best, W)
  best_obj <- obj_fun(best)
  
  for (i in 1:max_iter) {
    candidate <- best + rnorm(dim_s, mean = 0, sd = sigma)
    candidate <- pmin(pmax(candidate, lower), upper)
    score <- hill_wfun(candidate, W)
    if (score < best_score) {
      best <- candidate
      best_score <- score
      best_obj <- obj_fun(candidate)
    }
  }
  
  return(best_obj)
}

# 7) Executa para cada semana
resultados_semanais <- vector("list", Runs)

for (semana in 1:Runs) {
  cat(sprintf("Semana %d/%d\n", semana, Runs))
  
  # Atualizar previsão global
  pred_sales <- pred_list[[semana]]
  
  dim_s <- length(calc_upper(pred_sales))
  lower <- rep(0, dim_s)
  upper <- calc_upper(pred_sales)
  
  # Executa para todos os pesos
  resultados_W <- lapply(WTS, function(w) hill_climbing_simple(w, lower, upper, dim_s, MAXIT))
  M_RAW <- do.call(rbind, resultados_W)
  colnames(M_RAW) <- c("lucro_neg", "recursos")
  
  M_PT <- data.frame(
    lucro = -M_RAW[, "lucro_neg"],
    recursos = M_RAW[, "recursos"]
  )
  
  # Filtra Pareto
  M_PT <- paretoFilter(as.matrix(M_PT))
  M_PT <- as.data.frame(M_PT)
  colnames(M_PT) <- c("lucro", "recursos")
  M_PT <- M_PT[order(M_PT$recursos), ]
  
  # Hipervolume
  ref_pt <- c(max(M_PT$recursos) * 1.1, min(M_PT$lucro) * 0.9)
  hv <- dominatedHypervolume(as.matrix(M_PT[, c("recursos", "lucro")]), ref_pt)
  
  resultados_semanais[[semana]] <- list(
    pareto = M_PT,
    hv = hv
  )
  
  # Gráfico: Frente de Pareto
  plot(M_PT$recursos, M_PT$lucro, pch = 16,
       xlab = "Recursos", ylab = "Lucro",
       main = paste("Frente de Pareto - Semana", semana))
  lines(M_PT$recursos, M_PT$lucro, type = "l", col = "gray")
  
  cat(sprintf("Hypervolume (semana %d): %.2f\n", semana, hv))
}

# 8) Estatísticas finais
hvs <- sapply(resultados_semanais, function(r) r$hv)
cat("\nResumo dos hipervolumes:\n")
print(round(hvs, 2))
cat(sprintf("Média do Hipervolume: %.2f\n", mean(hvs)))
