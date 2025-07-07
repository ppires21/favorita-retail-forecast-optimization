library(rminer)

# 1. Parâmetros fixos
forecast_horizon <- 7  # previsão de 7 dias

# 2. Carregamento dos dados (remover apenas os últimos 3 dias)
sales <- read.csv("sales.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- sales[1:(nrow(sales) - 3), ]
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# 3. Melhores hiperparâmetros + growing window por série
melhores_params <- list(
  s_d11 = list(neurons=10, iters=200, lag_window=15, increment=7, Runs=10),
  s_d12 = list(neurons=15, iters=100, lag_window=15, increment=7, Runs=10),
  s_e11 = list(neurons=15, iters=100, lag_window=15, increment=7, Runs=10),
  s_e12 = list(neurons=10, iters=200, lag_window=15, increment=7, Runs=10),
  s_b11 = list(neurons=10, iters=200, lag_window=15, increment=7, Runs=10),
  s_b12 = list(neurons=10, iters=200, lag_window=15, increment=7, Runs=10)
)

# 4. Loop por cada série
for (col in sales_cols) {
  cat("\n=== Growing Window – Série:", col, "===\n")
  
  # 4.1. Obter a série
  serie_total <- sales_cleaned[[col]]
  serie <- tail(serie_total, 365)
  d1 <- as.numeric(serie)
  
  # 4.2. Parâmetros desta série
  p <- melhores_params[[col]]
  lag_window <- p$lag_window
  increment  <- p$increment
  Runs       <- p$Runs
  timelags   <- 1:lag_window
  W  <- 365 - forecast_horizon - (Runs - 1) * increment
  W2 <- W - lag_window
  
  cat("Parâmetros usados:\n"); print(p)
  
  # 4.3. Preparar dados com lags
  D  <- CasesSeries(d1, timelags)
  YR <- diff(range(d1))
  
  # 4.4. Vetores de métricas
  mae_v  <- numeric(Runs)
  nmae_v <- numeric(Runs)
  rmse_v <- numeric(Runs)
  rrse_v <- numeric(Runs)
  r2_v   <- numeric(Runs)
  
  # 4.5. Growing window – múltiplas previsões
  for (b in 1:Runs) {
    H1 <- holdout(d1, ratio=forecast_horizon, mode="incremental", iter=b, window=W, increment=increment)
    H2 <- holdout(D$y, ratio=forecast_horizon, mode="incremental", iter=b, window=W2, increment=increment)
    
    # Fit neural network model (MLP)
    M <- fit(y ~ ., D[H2$tr,], model="mlp", 
             neurons=p$neurons, maxit=p$iters)
    
    P <- lforecast(M, D, start=(length(H2$tr)+1), horizon=forecast_horizon)
    A <- d1[H1$ts]
    
    mae_v[b]  <- mmetric(A, P, metric="MAE")
    nmae_v[b] <- mmetric(A, P, metric="NMAE", val=YR)
    rmse_v[b] <- mmetric(A, P, metric="RMSE")
    rrse_v[b] <- mmetric(A, P, metric="RRSE")
    r2_v[b]   <- mmetric(A, P, metric="R2")
    
    cat(sprintf("Iteração %d – MAE=%.3f  NMAE=%.3f  RMSE=%.3f  RRSE=%.3f  R2=%.3f\n",
                b, mae_v[b], nmae_v[b], rmse_v[b], rrse_v[b], r2_v[b]))
  }
  
  # 4.6. Estatísticas finais
  cat(sprintf("MÉDIAS – MAE=%.3f  NMAE=%.3f  RMSE=%.3f  RRSE=%.3f  R2=%.3f\n",
              mean(mae_v), mean(nmae_v), mean(rmse_v), mean(rrse_v), mean(r2_v)))
  
  # 4.7. Gráfico da última previsão
  mgraph(A, P,
         graph="REG", Grid=10, col=c("black", "green"),
         leg=list(pos="topright", leg=c("real", "previsto")),
         main=paste("Growing MLP –", col))
}
