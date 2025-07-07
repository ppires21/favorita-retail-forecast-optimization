library(rminer)

# -------------------------------
# 2. Parâmetros da previsão
# -------------------------------
forecast_horizon <- 7
lag_window       <- 7

set.seed(123)
# -------------------------------
# 3. Hiperparâmetros ótimos por série
# -------------------------------
melhores_params <- list(
  s_d11 = list(size = 15, reps = 5),  # NMAE = 31.335
  s_d12 = list(size = 15, reps = 1),  # NMAE = 37.628
  s_e11 = list(size = 10, reps = 3),  # NMAE = 36.346
  s_e12 = list(size = 15, reps = 5),  # NMAE = 69.077
  s_b11 = list(size = 15, reps = 5),  # NMAE = 48.537
  s_b12 = list(size = 10, reps = 3)   # NMAE = 20.645
)

# -------------------------------
# 4. Carregamento dos dados
# -------------------------------
sales <- read.csv("sales.csv", sep = ";")
sales$date     <- as.Date(sales$date)
sales_cleaned  <- head(sales, -3)
sales_cols     <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# -------------------------------
# 5. Loop principal
# -------------------------------
for (col in sales_cols) {
  cat("\n=== Coluna:", col, "===\n")
  
  # 5.1. Série e preparação
  serie     <- sales_cleaned[[col]]
  train_ser <- head(serie, -forecast_horizon)
  serie_ext <- c(train_ser, rep(NA, forecast_horizon))
  
  # 5.2. Casos com lags
  D_ext     <- CasesSeries(serie_ext, 1:lag_window)
  train_idx <- which(!is.na(D_ext$y))
  start_idx <- max(train_idx) + 1
  actuals   <- tail(serie, forecast_horizon)
  
  # 5.3. Usar os parâmetros ótimos da série atual
  p <- melhores_params[[col]]
  
  # Treinando o modelo de rede neural
  model_full <- fit(
    y ~ ., data = D_ext[train_idx,],
    model = "mlpe",  # Utiliza a rede neural
    size = p$size,   # Número de neurônios na camada oculta
    reps = p$reps,   # Número de repetições para reduzir a variabilidade
    maxit = 200      # Número de iterações (pode ser ajustado conforme necessário)
  )
  
  # 5.4. Previsão multi‑step
  preds <- lforecast(model_full, D_ext, start = start_idx, horizon = forecast_horizon)
  
  # 5.5. Avaliação
  srange <- diff(range(actuals, na.rm = TRUE))
  mae    <- mmetric(actuals, preds, metric = "MAE")
  nmae   <- mmetric(actuals, preds, metric = "NMAE", val = srange)
  rmse   <- mmetric(actuals, preds, metric = "RMSE")
  rrse   <- mmetric(actuals, preds, metric = "RRSE")
  
  cat("\nParâmetros usados:\n")
  print(p)
  
  cat("\nMétricas:\n")
  cat(sprintf(" MAE = %.3f\n", mae))
  cat(sprintf(" NMAE = %.3f\n", nmae))
  cat(sprintf(" RMSE = %.3f\n", rmse))
  cat(sprintf(" RRSE = %.3f\n\n", rrse))
  
  # 5.6. Gráfico
  plot(
    1:forecast_horizon, actuals,
    type = "b", pch = 16,
    xlab = "Dia", ylab = "Valor",
    main = paste("Rede Neural Tunada –", col),
    col  = "red", ylim = range(c(actuals, preds), na.rm = TRUE)
  )
  lines(
    1:forecast_horizon, preds,
    type = "b", pch = 17, col = "darkgreen"
  )
  legend(
    "topright",
    legend = c("Real", "Neural Network"),
    col    = c("red", "darkgreen"),
    pch    = c(16, 17)
  )
}
