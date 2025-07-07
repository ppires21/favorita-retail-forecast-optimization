library(rminer)

# -------------------------------
# 2. Parâmetros da previsão
# -------------------------------
forecast_horizon <- 7     # Número de dias a prever
lag_window       <- 7     # Número de lags usados como inputs

# -------------------------------
# 3. Grelha de hiperparâmetros a testar (RF)
# -------------------------------
grid <- expand.grid(
  ntree = c(50, 100, 200),  # nº de árvores
  mtry  = c(2, 4, 6)          # variáveis por split
)

# -------------------------------
# 4. Carregamento dos dados
# -------------------------------
sales <- read.csv("C:/Users/joaoa/OneDrive/Documentos/Rstudio/sales_clean.csv", sep = ";")
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
  
  # 5.3. Tuning manual baseado no NMAE
  melhor_nmae   <- Inf
  melhor_params <- NULL
  melhor_modelo <- NULL
  
  for (i in seq_len(nrow(grid))) {
    params <- grid[i, ]
    
    # Treinar modelo RF
    modelo <- fit(
      y ~ ., data = D_ext[train_idx,],
      model   = "randomForest",
      ntree   = params$ntree,
      mtry    = params$mtry
    )
    
    # Prever com lforecast
    preds <- lforecast(modelo, D_ext, start = start_idx, horizon = forecast_horizon)
    
    # Se falhar, ignorar
    if (all(is.na(preds))) next
    
    # Avaliar com NMAE
    srange   <- diff(range(actuals, na.rm = TRUE))
    nmae_val <- mmetric(actuals, preds, metric = "NMAE", val = srange)
    cat(sprintf("Testado: ntree=%4d, mtry=%d → NMAE=%.3f\n",
                params$ntree, params$mtry, nmae_val))
    
    if (!is.na(nmae_val) && nmae_val < melhor_nmae) {
      melhor_nmae   <- nmae_val
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
  cat(sprintf(" MAE   = %.3f\n", mae))
  cat(sprintf(" NMAE  = %.3f\n", nmae))
  cat(sprintf(" RMSE  = %.3f\n", rmse))
  cat(sprintf(" RRSE  = %.3f\n\n", rrse))
  
  # 5.6. Gráfico
  plot(
    1:forecast_horizon, actuals,
    type = "b", pch = 16,
    xlab = "Dias", ylab = "Valor",
    main = paste("RF Tunado –", col),
    ylim = range(c(actuals, preds), na.rm = TRUE)
  )
  lines(
    1:forecast_horizon, preds,
    type = "b", pch = 17
  )
  legend(
    "topright",
    legend = c("Real", "RF Previsto"),
    pch    = c(16, 17)
  )
}
