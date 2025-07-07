# differential_evolution_fase2_updated.R

# 0) Pacotes e fontes
library(rminer)
library(forecast)
library(DEoptim)

# funções auxiliares: repair(), eval(), calc_upper()
source("otiutils.R")    

set.seed(12345)

# 1) Carregamento e limpeza dos dados
sales <- read.csv("sales_clean.csv", sep=";")
sales$date <- as.Date(sales$date)
sales_cleaned <- sales[1:(nrow(sales)-3), ]
sales_cols    <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")

# 2) Parâmetros gerais de Growing Window
Runs             <- 10      # iterações / semanas de teste
forecast_horizon <- 7
lag_window       <- 15
increment        <- 7
NP      <- 50
itermax <- 100
W  <- 365 - forecast_horizon - (Runs-1)*increment
W2 <- W - lag_window

# hiperparâmetros XGBoost (igual ao Monte Carlo)
params_xgb <- list(
  s_d11 = list(eta=0.05, max_depth=6, nrounds=50),
  s_d12 = list(eta=0.05, max_depth=4, nrounds=150),
  s_e11 = list(eta=0.20, max_depth=6, nrounds=150),
  s_e12 = list(eta=0.05, max_depth=6, nrounds=50),
  s_b11 = NULL,  # ARIMA
  s_b12 = list(eta=0.20, max_depth=6, nrounds=50)
)

# 3) Função de forecasting por série e iteração
forecast_series <- function(col, b) {
  serie_total <- sales_cleaned[[col]]
  serie       <- tail(serie_total, 365)
  d1          <- as.numeric(serie)
  
  H1 <- holdout(d1,
                ratio     = forecast_horizon,
                mode      = "incremental",
                iter      = b,
                window    = W,
                increment = increment)
  
  if (col == "s_b11") {
    ts_train  <- ts(d1[H1$tr], frequency = 7)
    fit_arima <- auto.arima(ts_train)
    P <- as.numeric(forecast(fit_arima, h=forecast_horizon)$mean)
  } else {
    p     <- params_xgb[[col]]
    Dset  <- CasesSeries(d1, 1:lag_window)
    H2    <- holdout(Dset$y,
                     ratio     = forecast_horizon,
                     mode      = "incremental",
                     iter      = b,
                     window    = W2,
                     increment = increment)
    model <- fit(y ~ ., Dset[H2$tr,],
                 model     = "xgboost",
                 eta       = p$eta,
                 max_depth = p$max_depth,
                 nrounds   = p$nrounds,
                 objective = "reg:squarederror")
    P <- lforecast(model, Dset,
                   start   = length(H2$tr) + 1,
                   horizon = forecast_horizon)
  }
  return(P)
}

# 4) Gera lista de matrizes 7×6 de previsões
pred_list <- lapply(1:Runs, function(b) {
  mat <- matrix(NA, nrow=forecast_horizon, ncol=length(sales_cols),
                dimnames=list(NULL, sales_cols))
  for (j in seq_along(sales_cols))
    mat[, j] <- forecast_series(sales_cols[j], b)
  mat
})

# 5) Otimização por Differential Evolution (DEoptim) com histórico
optimiza_semana_DE <- function(pred_matrix, NP=50, itermax=100) {
  assign("pred_sales", pred_matrix, envir = .GlobalEnv)
  lower <- rep(0, 70)
  upper <- calc_upper(pred_sales)
  
  # ativa gravação de histórico de bestval
  ctrl <- DEoptim.control(NP=NP, itermax=itermax, F=0.8, CR=0.9,
                          trace=FALSE, storepopfrom=1)
  
  res <- DEoptim(fn = function(s) -eval(repair(s)),
                 lower = lower, upper = upper,
                 control = ctrl)
  
  best_mem    <- res$optim$bestmem
  lucro       <- -res$optim$bestval
  # profit_hist: histórico do valor ótimo (invertido) por iteração
  hist_best   <- -res$member$bestvalit
  list(lucro = lucro, sol = round(best_mem), profit_hist = hist_best)
}

# 6) Loop sobre as semanas + agregação + convergência
run_phase2_DE <- function(pred_list, NP=50, itermax=100, aggr=c("mean","median")) {
  aggr <- match.arg(aggr)
  n     <- length(pred_list)
  lucros <- numeric(n)
  sols   <- vector("list", n)
  histos <- vector("list", n)
  
  for (i in seq_along(pred_list)) {
    cat(sprintf("Semana %2d/%d — ", i, n))
    out <- optimiza_semana_DE(pred_list[[i]], NP=NP, itermax=itermax)
    cat(sprintf("Lucro = %.2f EUR
", out$lucro))
    lucros[i] <- out$lucro
    sols[[i]] <- out$sol
    histos[[i]] <- out$profit_hist
  }
  
  lucro_agr <- if (aggr=="mean") mean(lucros) else median(lucros)
  cat("============================================
")
  cat(sprintf("%s dos lucros = %.2f EUR
", toupper(aggr), lucro_agr))
  
  list(lucros=lucros, lucro_agr=lucro_agr, sols=sols, profit_hist_list=histos)
}

# 7) Executa e gera resultados
resultados_DE <- run_phase2_DE(pred_list, NP=50, itermax=100, aggr="median")

# 8) Tabela de resultados
df_res_DE <- data.frame(
  Metodo        = "DifferentialEvolution",
  Semana        = seq_along(resultados_DE$lucros),
  Lucro         = resultados_DE$lucros,
  LucroAgregado = resultados_DE$lucro_agr
)

cat("\n--- Tabela de Lucros por Semana (DE) ---\n")
print(df_res_DE)
cat(sprintf("\nLucro agregado (mediana) = %.2f EUR\n\n", resultados_DE$lucro_agr))

# 9) Gráfico de Lucros por Semana
plot(df_res_DE$Semana, df_res_DE$Lucro,
     type  = "b", pch=16,
     xlab  = "Semana",
     ylab  = "Lucro (EUR)",
     main  = "Lucro por Semana — Differential Evolution (Fase 2)")
abline(h = resultados_DE$lucro_agr, lty=2)
legend("topright",
       legend = sprintf("Mediana = %.2f", resultados_DE$lucro_agr),
       lty    = 2, bty="n")

# → matriz de históricos (cada coluna = 1 semana, cada linha = 1 geração)
profit_mat <- do.call(cbind, resultados_DE$profit_hist_list)

# → criar vetor de número acumulado de avaliações
gen      <- seq_len(itermax)        # 1,2,…,100 gerações
eval_counts    <- gen * NP   

# 10) Gráfico de Convergência — todas as semanas
profit_mat <- do.call(cbind, resultados_DE$profit_hist_list)
matplot(
  eval_counts,
  profit_mat,
  type  = "l", lty = 1, lwd = 1.5,
  xlab  = "Número de Avaliações",
  ylab  = "Lucro (EUR)",
  main  = "Convergência do DE — Fase 2",
  col   = rainbow(ncol(profit_mat))
)
legend(
  "bottomright",
  legend = paste("Semana", seq_len(ncol(profit_mat))),
  col    = rainbow(ncol(profit_mat)),
  lty    = 1, bty = "n", cex = 0.8
)
