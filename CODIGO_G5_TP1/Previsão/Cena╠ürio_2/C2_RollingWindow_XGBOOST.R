# rolling_lastyear_xgboost.R: previsão univariada multi-step com rolling window otimizado
library(rminer)

# 1. Parâmetros fixos
forecast_horizon <- 7  # previsão de 7 dias

# 2. Carregamento dos dados (remover apenas os últimos 3 dias)
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- sales[1:(nrow(sales) - 3), ]
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# 3. Melhores hiperparâmetros + rolling window por série
melhores_params <- list(
  s_d11 = list(eta=0.05,  max_depth=4, nrounds=50,  lag_window=15,  increment=7,  Runs=10),
  s_d12 = list(eta=0.05,  max_depth=4, nrounds=50,  lag_window=15, increment=7,  Runs=10),
  s_e11 = list(eta=0.1,  max_depth=4, nrounds=50,  lag_window=15,  increment=7,  Runs=10),
  s_e12 = list(eta=0.05,  max_depth=4, nrounds=50,  lag_window=15, increment=7, Runs=10),
  s_b11 = list(eta=0.05,   max_depth=4, nrounds=100,  lag_window=15, increment=7, Runs=10),
  s_b12 = list(eta=0.05,   max_depth=4, nrounds=100,  lag_window=15,  increment=7,  Runs=10)
)

# 4. Loop por cada série
for (col in sales_cols) {
  cat("\n=== Rolling Window – Série:", col, "===\n")
  
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
  
  # 4.5. Rolling window – múltiplas previsões
  for (b in 1:Runs) {
    H1 <- holdout(d1, ratio=forecast_horizon, mode="rolling", iter=b, window=W, increment=increment)
    H2 <- holdout(D$y, ratio=forecast_horizon, mode="rolling", iter=b, window=W2, increment=increment)
    
    M <- fit(y ~ ., D[H2$tr,], model="xgboost",
             eta=p$eta, max_depth=p$max_depth, nrounds=p$nrounds,
             objective="reg:squarederror")
    
    P <- lforecast(M, D, start=(length(H2$tr)+1), horizon=forecast_horizon)
    A <- d1[H1$ts]
    
    mae_v[b]  <- mmetric(A, P, metric="MAE")
    nmae_v[b] <- mmetric(A, P, metric="NMAE", val=YR)
    rmse_v[b] <- mmetric(A, P, metric="RMSE")
    rrse_v[b] <- mmetric(A, P, metric="RRSE")
    
    cat(sprintf("Iteração %d – MAE=%.3f  NMAE=%.3f  RMSE=%.3f  RRSE=%.3f\n",
                b, mae_v[b], nmae_v[b], rmse_v[b], rrse_v[b]))
  }
  
  # 4.6. Previsões da última iteração (n+1 a n+7)
  cat("\nPrevisões da última iteração:\n")
  for (i in 1:forecast_horizon) {
    cat(sprintf("  Dia n+%d: %.2f\n", i, P[i]))
  }
  
  # 4.7. Estatísticas finais (médias)
  cat(sprintf("\nMÉDIAS – MAE=%.3f  NMAE=%.3f  RMSE=%.3f  RRSE=%.3f\n",
              mean(mae_v), mean(nmae_v), mean(rmse_v), mean(rrse_v)))
  
  # 4.8. Gráfico da última previsão
  mgraph(A, P,
         graph="REG", Grid=10, col=c("black", "green"),
         leg=list(pos="topright", leg=c("real", "previsto")),
         main=paste("Rolling XGBoost –", col))
}
