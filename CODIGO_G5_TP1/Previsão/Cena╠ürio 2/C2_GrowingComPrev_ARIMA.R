# C2_GrowingWindow_ARIMA_with_7day_forecast.R: previsão univariada multi-step com growing window usando ARIMA
library(rminer)
library(forecast)

# 1. Parâmetros fixos
forecast_horizon <- 7   # horizonte de previsão (7 dias)
lagwindow       <- 15   # tamanho da janela inicial (lagwindow)

# 2. Carregamento dos dados
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- sales[1:(nrow(sales) - 3), ]
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# 3. Melhores parâmetros para cada série
melhores_params <- list(
  s_d11 = list(Runs = 10, increment = 7),
  s_d12 = list(Runs = 10, increment = 7),
  s_e11 = list(Runs = 10, increment = 7),
  s_e12 = list(Runs = 10, increment = 7),
  s_b11 = list(Runs = 10, increment = 7),
  s_b12 = list(Runs = 10, increment = 7)
)

# 4. Loop por cada série
for (col in sales_cols) {
  cat("\n=== Growing Window – Série:", col, "===\n")
  
  # Construir vetor da última série
  serie_total <- sales_cleaned[[col]]
  serie <- tail(serie_total, 365)
  d1 <- as.numeric(serie)
  
  # Parâmetros desta série
  p         <- melhores_params[[col]]
  Runs      <- p$Runs           # número de iterações
  increment <- p$increment      # incremento em dias por iteração
  W         <- lagwindow        # janela inicial fixa de 15 lags
  
  # Vetores para armazenar métricas
  mae_v  <- numeric(Runs)
  nmae_v <- numeric(Runs)
  rmse_v <- numeric(Runs)
  rrse_v <- numeric(Runs)
  YR     <- diff(range(d1))     # para normalização da NMAE
  
  # 5. Growing window – múltiplas previsões
  for (b in 1:Runs) {
    # holdout com janela inicial fixa (= lagwindow)
    H1 <- holdout(
      d1,
      ratio     = forecast_horizon,
      mode      = "incremental",
      iter      = b,
      window    = W, 
      increment = increment
    )
    
    # Ajuste ARIMA na série de treino
    train_ts    <- ts(d1[H1$tr], frequency = 7)
    model_arima <- auto.arima(train_ts)
    fc          <- forecast(model_arima, h = forecast_horizon)
    
    P <- as.numeric(fc$mean)  # previsões
    A <- d1[H1$ts]            # observados
    
    # Métricas
    mae_v[b]  <- mmetric(A, P, metric = "MAE")
    nmae_v[b] <- mmetric(A, P, metric = "NMAE", val = YR)
    rmse_v[b] <- mmetric(A, P, metric = "RMSE")
    rrse_v[b] <- mmetric(A, P, metric = "RRSE")
    
    cat(sprintf(
      "Iteração %d – MAE=%.3f  NMAE=%.3f  RMSE=%.3f  RRSE=%.3f\n",
      b, mae_v[b], nmae_v[b], rmse_v[b], rrse_v[b]
    ))
  }
  
  # Resultados agregados
  cat("\nMedianas das métricas para série", col, ":\n")
  cat("MAE mediano:   ", median(mae_v),  "\n")
  cat("NMAE mediano:  ", median(nmae_v), "\n")
  cat("RMSE mediano:  ", median(rmse_v), "\n")
  cat("RRSE mediano:  ", median(rrse_v), "\n")
  
  # 6. Ajuste final e previsão para os próximos 7 dias
  full_ts    <- ts(serie_total, frequency = 7)
  model_full <- auto.arima(full_ts)
  fc_full    <- forecast(model_full, h = forecast_horizon)
  
  # Preparar datas futuras
  last_date <- max(sales_cleaned$date)
  future_dates <- seq(last_date + 1, by = "day", length.out = forecast_horizon)
  forecast_df <- data.frame(
    date     = future_dates,
    forecast = as.numeric(fc_full$mean)
  )
  
  cat("\nPrevisão para os próximos 7 dias da série", col, ":\n")
  print(forecast_df)
}
