library(rminer)

# -------------------------------
# 2. Parâmetros da previsão
# -------------------------------
forecast_horizon <- 7     # Número de dias a prever
lag_window       <- 7     # Número de lags usados como inputs

# -------------------------------
# 3. Grelha de hiperparâmetros a testar
# -------------------------------
grid <- expand.grid(
  eta       = c(0.05, 0.1, 0.2),
  max_depth = c(4, 5, 6),
  nrounds   = c(50, 100, 150, 200)
)

# -------------------------------
# 4. Carregamento dos dados
# -------------------------------
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date     <- as.Date(sales$date)
sales_cleaned  <- head(sales, -3)
sales_cols     <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")

# -------------------------------
# 5. Loop principal
# -------------------------------
for (col in sales_cols) {
  cat("\n=== Coluna:", col, "===\n")
  
  # 5.1. Preparar série
  serie     <- sales_cleaned[[col]]
  train_ser <- head(serie, -forecast_horizon)
  serie_ext <- c(train_ser, rep(NA, forecast_horizon))
  
  # 5.2. Criar data.frame com lags
  D_ext     <- CasesSeries(serie_ext, 1:lag_window)
  train_idx <- which(!is.na(D_ext$y))
  start_idx <- max(train_idx) + 1
  actuals   <- tail(serie, forecast_horizon)
  
  # 5.3. Tunning manual baseado no NMAE de lforecast    # <— alterado
  melhor_nmae   <- Inf                                 # <— alterado (era melhor_rmse)
  melhor_params <- NULL
  melhor_modelo <- NULL
  
  for (i in 1:nrow(grid)) {
    params <- grid[i, ]
    
    # Treinar modelo
    modelo <- fit(
      y ~ ., data = D_ext[train_idx,],
      model = "xgboost",
      task = "reg",
      eta = params$eta,
      max_depth = params$max_depth,
      nrounds = params$nrounds
    )
    
    # Prever com lforecast
    preds <- lforecast(modelo, D_ext, start = start_idx, horizon = forecast_horizon)
    
    # Se falhar, ignorar
    if (all(is.na(preds))) next
    
    # Avaliar com NMAE                                 # <— alterado (era RMSE)
    srange   <- diff(range(actuals, na.rm = TRUE))      # <— movido/copiado para cálculo de NMAE
    nmae_val <- mmetric(actuals, preds, metric = "NMAE", val = srange)  # <— alterado
    cat(sprintf("Testado: eta=%.2f, depth=%d, nrounds=%d → NMAE=%.3f\n", 
                params$eta, params$max_depth, params$nrounds, nmae_val))    # <— alterado
    
    if (!is.na(nmae_val) && nmae_val < melhor_nmae) {    # <— alterado (comparação com NMAE)
      melhor_nmae   <- nmae_val                         # <— alterado
      melhor_params <- params
      melhor_modelo <- modelo
    }
  }
  
  # 5.4. Usar melhor modelo e gerar previsão final
  if (is.null(melhor_modelo)) {
    warning(paste("Não foi possível gerar previsões para", col))
    next
  }
  
  preds <- lforecast(melhor_modelo, D_ext, start = start_idx, horizon = forecast_horizon)
  
  # 5.5. Avaliação
  srange <- diff(range(actuals, na.rm = TRUE))
  mae    <- mmetric(actuals, preds, metric = "MAE")
  nmae   <- mmetric(actuals, preds, metric = "NMAE", val = srange)
  rmse   <- mmetric(actuals, preds, metric = "RMSE")
  rrse   <- mmetric(actuals, preds, metric = "RRSE")
  
  cat("\nMelhores parâmetros:\n")
  print(melhor_params)
  
  cat("\nMétricas:\n")
  cat(sprintf(" MAE = %.3f\n", mae))
  cat(sprintf(" NMAE = %.3f\n", nmae))
  cat(sprintf(" RMSE = %.3f\n", rmse))
  cat(sprintf(" RRSE = %.3f\n\n", rrse))
  
  # 5.6. Gráfico
  plot(
    1:forecast_horizon, actuals,
    type = "b", pch = 16,
    xlab = "Dias", ylab = "Valor",
    main = paste("XGBoost Tunado –", col),
    col  = "red", ylim = range(c(actuals, preds), na.rm = TRUE)
  )
  lines(
    1:forecast_horizon, preds,
    type = "b", pch = 17, col = "darkgreen"
  )
  legend(
    "topright",
    legend = c("Real", "XGBoost"),
    col    = c("red", "darkgreen"),
    pch    = c(16, 17)
  )
}
