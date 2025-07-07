# C2_RollingWindow_ARIMA.R: previsão univariada multi-step com rolling window usando ARIMA

library(rminer)
library(forecast)

# 1. Parâmetros fixos
forecast_horizon <- 7  # previsão de 7 dias
lag_window <- 15       # tamanho da janela de lag fixa

# 2. Carregamento dos dados
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- sales[1:(nrow(sales) - 3), ]
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# 3. Melhores hiperparâmetros + rolling window por série
melhores_params <- list(
  s_d11 = list(increment=7,  Runs=10),
  s_d12 = list(increment=7,  Runs=10),
  s_e11 = list(increment=7,  Runs=10),
  s_e12 = list(increment=7,  Runs=10),
  s_b11 = list(increment=7,  Runs=10),
  s_b12 = list(increment=7,  Runs=10)
)

# 4. Loop por cada série
for (col in sales_cols) {
  cat("\n=== Rolling Window – Série:", col, "===\n")
  
  # 4.1. Obter a série dos últimos 365 dias
  serie <- tail(sales_cleaned[[col]], 365)
  d1 <- as.numeric(serie)
  
  # 4.2. Parâmetros desta série
  p <- melhores_params[[col]]
  Runs     <- p$Runs
  increment <- p$increment
  W <- lag_window  # usa janela de lag fixa
  
  cat("Parâmetros usados (lag_window, incremento, Runs):", W, ",", increment, ",", Runs, "\n")
  
  # 4.3. Vetores de métricas
  mae_v  <- numeric(Runs)
  nmae_v <- numeric(Runs)
  rmse_v <- numeric(Runs)
  rrse_v <- numeric(Runs)
  YR <- diff(range(d1))
  
  # 4.4. Rolling window – múltiplas previsões
  for (b in 1:Runs) {
    H <- holdout(d1, ratio=forecast_horizon, mode="rolling", iter=b,
                 window=W, increment=increment)
    
    # Treino e ajuste ARIMA
    ts_train <- ts(d1[H$tr], frequency=7)
    arima_model <- auto.arima(ts_train)
    fcast <- forecast(arima_model, h = forecast_horizon)
    
    P <- as.numeric(fcast$mean)
    A <- d1[H$ts]
    
    # 4.5. Cálculo das métricas
    mae_v[b]  <- mmetric(A, P, metric="MAE")
    nmae_v[b] <- mmetric(A, P, metric="NMAE", val = YR)
    rmse_v[b] <- mmetric(A, P, metric="RMSE")
    rrse_v[b] <- mmetric(A, P, metric="RRSE")
    
    cat(sprintf("Iteração %d – MAE=%.3f  NMAE=%.3f  RMSE=%.3f  RRSE=%.3f\n",
                b, mae_v[b], nmae_v[b], rmse_v[b], rrse_v[b]))
  }
  
  # 4.6. Estatísticas finais
  cat(sprintf("MÉDIAS :\n  MAE=%.3f\n  NMAE=%.3f\n  RMSE=%.3f\n  RRSE=%.3f\n",
              mean(mae_v), mean(nmae_v), mean(rmse_v), mean(rrse_v)))
  
  # 4.7. Gráfico da última previsão
  mgraph(A, P,
         graph = "REG", Grid = 10,
         col = c("black", "green"),
         leg = list(pos = "topright", leg = c("real", "previsto")),
         main = paste("Rolling ARIMA –", col))
}
