# ----------------------------------------
# Hill Climbing com Análise de Convergência
# ----------------------------------------

# 0) Pacotes e fontes
library(rminer)
library(forecast)

source("otiutils.R")  # funções: repair(), eval(), calc_upper()
source("hill.R")      # função: hclimbing()

set.seed(12345)

# 1) Leitura das previsões previamente guardadas
forecast_df <- read.csv("previsoes_10_iteracoes.csv")
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")
forecast_horizon <- 7
Runs <- 10

# 2) Reorganiza o dataframe em lista de matrizes 7×6
pred_list <- vector("list", Runs)
for (i in 1:Runs) {
  cols_i <- paste0(sales_cols, "_", i)
  mat_i <- as.matrix(forecast_df[1:forecast_horizon, cols_i])
  colnames(mat_i) <- sales_cols
  pred_list[[i]] <- mat_i
}

# 3) Variáveis globais para análise de convergência
TYPE <- "max"               # queremos maximizar o lucro
MAXIT <- 5000              # número de iterações do hill climbing
EV <- 0                     # contador de avaliações
BEST <- -Inf                # melhor lucro observado até agora
F <- rep(NA, MAXIT)         # vetor para guardar o histórico do melhor lucro

# 4) Função de avaliação monitorada
m_eval <- function(s) {
  s_reparado <- repair(s)
  res <- eval(s_reparado)
  
  EV <<- EV + 1
  BEST <<- max(BEST, res)
  if (EV <= MAXIT) F[EV] <<- BEST
  
  return(res)
}

# 5) Função de mutação customizada
rchange_custom <- function(par, lower, upper) {
  par_new <- par
  delta <- rnorm(42, mean = 0, sd = 10)
  par_new[29:70] <- par_new[29:70] + delta
  return(par_new)
}

# 6) Função de otimização Hill Climbing com análise de convergência
optimiza_semana <- function(pred_matrix, N = 5000) {
  assign("pred_sales", pred_matrix, envir = .GlobalEnv)
  
  # Reinicia variáveis globais de convergência
  assign("EV", 0, envir = .GlobalEnv)
  assign("BEST", -Inf, envir = .GlobalEnv)
  assign("MAXIT", N, envir = .GlobalEnv)
  assign("F", rep(NA, N), envir = .GlobalEnv)
  
  lower <- rep(0, 70)
  upper <- calc_upper(pred_sales)
  
  initial_par <- rep(0, 70)
  initial_par[29:70] <- as.vector(t(pred_sales))
  initial_par <- repair(initial_par)
  
  res <- hclimbing(
    par = initial_par,
    fn = function(s) m_eval(s),  # avaliação monitorada
    change = rchange_custom,
    lower = lower,
    upper = upper,
    control = list(maxit = N, REPORT = 100, digits = 0),
    type = "max"
  )
  
  list(lucro = res$eval,
       sol   = round(res$sol),
       historico = F)
}

# 7) Executa para todas as semanas
run_phase2 <- function(pred_list, N = 5000, aggr = c("mean", "median")) {
  aggr   <- match.arg(aggr)
  lucros <- numeric(length(pred_list))
  sols   <- vector("list", length(pred_list))
  historicos <- matrix(NA, nrow = N, ncol = length(pred_list))
  
  for (i in seq_along(pred_list)) {
    cat(sprintf("Semana %2d/%d — ", i, length(pred_list)))
    out <- optimiza_semana(pred_list[[i]], N = N)
    cat(sprintf("Lucro = %.2f EUR\n", out$lucro))
    lucros[i] <- out$lucro
    sols[[i]] <- out$sol
    historicos[, i] <- out$historico
  }
  
  lucro_agr <- if (aggr == "mean") mean(lucros) else median(lucros)
  cat("============================================\n")
  cat(sprintf("%s dos lucros = %.2f EUR\n", toupper(aggr), lucro_agr))
  
  list(lucros = lucros,
       lucro_agr = lucro_agr,
       sols = sols,
       historicos = historicos)
}

# 8) Executar e montar dataframe de resultados
resultados <- run_phase2(pred_list, N = 5000, aggr = "median")

df_res <- data.frame(
  Metodo        = "HillClimbing",
  Semana        = seq_along(resultados$lucros),
  Lucro         = resultados$lucros,
  LucroAgregado = resultados$lucro_agr
)

# 9) Imprime resultados
cat("\n\n--- Tabela de Lucros por Semana ---\n")
print(df_res)

cat(sprintf("\nLucro agregado (mediana) = %.2f EUR\n\n", resultados$lucro_agr))

# 10) Gráfico geral de lucros
plot(df_res$Semana, df_res$Lucro,
     type = "b",
     pch = 16,
     xlab = "Semana",
     ylab = "Lucro (EUR)",
     main = "Lucro por Semana — Hill Climbing (Fase 2)")
abline(h = resultados$lucro_agr, lty = 2)
legend("topright",
       legend = sprintf("Mediana = %.2f", resultados$lucro_agr),
       lty    = 2,
       bty    = "n")

# 11) Gráfico de convergência — todas as semanas
matplot(
  resultados$historicos,
  type = "l", lty = 1, lwd = 1.5,
  xlab = "Iteração",
  ylab = "Lucro (EUR)",
  main = "Convergência do Hill Climbing — Fase 2",
  col  = rainbow(ncol(resultados$historicos))
)

legend("bottomright",
       legend = paste("Semana", seq_len(ncol(resultados$historicos))),
       col    = rainbow(ncol(resultados$historicos)),
       lty    = 1, bty = "n", cex = 0.8)
