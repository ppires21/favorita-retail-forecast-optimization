# 1.CENARIO_UNI_ARIMA_AUTO.R: univariado multi-step forecasting usando auto.arima

library(rminer)
library(forecast)

# 1. Parâmetros da previsão
forecast_horizon <- 7     # Número de dias a prever

# 2. Carregamento dos dados
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date    <- as.Date(sales$date)
sales_cleaned <- head(sales, -3)
sales_cols    <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")

# 3. Loop principal por série usando auto.arima
for (col in sales_cols) {
  cat("\n=== Série:", col, "===\n")
  
  # 3.1 Preparar série de treino e valores reais
  serie     <- sales_cleaned[[col]]
  train_ser <- head(serie, -forecast_horizon)
  actuals   <- tail(serie, forecast_horizon)
  ts_train  <- ts(train_ser, frequency = 7)
  
  # 3.2 Ajuste automático ARIMA
  modelo <- auto.arima(ts_train)
  ord <- arimaorder(modelo)
  cat(sprintf("Modelo ARIMA selecionado: (%d,%d,%d)\n", ord["p"], ord["d"], ord["q"]))
  
  # 3.3 Previsão multi-step à frente
  fcast <- forecast(modelo, h = forecast_horizon)
  preds <- as.numeric(fcast$mean)
  
  # 3.4 Cálculo das métricas
  srange <- diff(range(actuals, na.rm = TRUE))
  mae    <- mmetric(actuals, preds, metric = "MAE")
  nmae   <- mmetric(actuals, preds, metric = "NMAE", val = srange)
  rmse   <- mmetric(actuals, preds, metric = "RMSE")
  rrse   <- mmetric(actuals, preds, metric = "RRSE")
  
  cat("\nMétricas de desempenho para", col, ":\n")
  cat(sprintf(" MAE  = %.3f\n", mae))
  cat(sprintf(" NMAE = %.3f\n", nmae))
  cat(sprintf(" RMSE = %.3f\n", rmse))
  cat(sprintf(" RRSE = %.3f\n", rrse))
  
  # 3.5 Gráfico das previsões
  plot(
    1:forecast_horizon, actuals,
    type = "b", pch = 16,
    xlab = "Dias", ylab = "Valor",
    main = paste("auto.arima –", col),
    col  = "red", ylim = range(c(actuals, preds), na.rm = TRUE)
  )
  lines(
    1:forecast_horizon, preds,
    type = "b", pch = 17, col = "darkgreen"
  )
  legend(
    "topright",
    legend = c("Real", "Previsto"),
    col    = c("red", "darkgreen"),
    pch    = c(16, 17)
  )
}
