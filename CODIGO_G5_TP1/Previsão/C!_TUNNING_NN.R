library(rminer)

# -------------------------------
# 1. Parâmetros
# -------------------------------
forecast_horizon <- 7
lag_window       <- 7
set.seed(123)

# -------------------------------
# 2. Grelha de hiperparâmetros para MLP
# -------------------------------
grid <- expand.grid(
  neurons = c(20, 30, 40),
  iters   = c(200, 300, 400)
)

# -------------------------------
# 3. Carregamento dos dados
# -------------------------------
sales <- read.csv("sales.csv", sep = ";")
sales$date     <- as.Date(sales$date)
sales_cleaned  <- head(sales, -3)
sales_cols     <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# -------------------------------
# 4. Loop principal por série
# -------------------------------
for (col in sales_cols) {
  cat("\n=== Série:", col, "===\n")
  
  # 4.1 Preparar série
  serie     <- sales_cleaned[[col]]
  train_ser <- head(serie, -forecast_horizon)
  serie_ext <- c(train_ser, rep(NA, forecast_horizon))
  
  # 4.2 Criar dataset com lags
  D_ext     <- CasesSeries(serie_ext, 1:lag_window)
  train_idx <- which(!is.na(D_ext$y))
  start_idx <- max(train_idx) + 1
  actuals   <- tail(serie, forecast_horizon)
  
  # 4.3 Tuning por NMAE
  melhor_nmae   <- Inf
  melhor_params <- NULL
  melhor_modelo <- NULL
  
  for (i in 1:nrow(grid)) {
    params <- grid[i, ]
    
    modelo <- fit(
      y ~ ., data = D_ext[train_idx,],
      model = "mlp",
      neurons = params$neurons,
      iters   = params$iters
    )
    
    preds <- lforecast(modelo, D_ext, start = start_idx, horizon = forecast_horizon)
    
    if (all(is.na(preds))) next
    
    srange   <- diff(range(actuals, na.rm = TRUE))
    nmae_val <- mmetric(actuals, preds, metric = "NMAE", val = srange)
    
    cat(sprintf("Testado: neurons=%d, iters=%d → NMAE=%.3f\n", 
                params$neurons, params$iters, nmae_val))
    
    if (!is.na(nmae_val) && nmae_val < melhor_nmae) {
      melhor_nmae   <- nmae_val
      melhor_params <- params
      melhor_modelo <- modelo
    }
  }
  
  # 4.4 Previsão final e avaliação
  if (is.null(melhor_modelo)) {
    warning(paste("Não foi possível gerar previsões para", col))
    next
  }
  
  preds <- lforecast(melhor_modelo, D_ext, start = start_idx, horizon = forecast_horizon)
  
  srange <- diff(range(actuals, na.rm = TRUE))
  mae    <- mmetric(actuals, preds, metric = "MAE")
  nmae   <- mmetric(actuals, preds, metric = "NMAE", val = srange)
  rmse   <- mmetric(actuals, preds, metric = "RMSE")
  rrse   <- mmetric(actuals, preds, metric = "RRSE")
  
  cat("\nMelhores parâmetros:\n")
  print(melhor_params)
  
  cat("\nMétricas:\n")
  cat(sprintf(" MAE  = %.3f\n", mae))
  cat(sprintf(" NMAE = %.3f\n", nmae))
  cat(sprintf(" RMSE = %.3f\n", rmse))
  cat(sprintf(" RRSE = %.3f\n\n", rrse))
  
  # 4.5 Gráfico
  plot(
    1:forecast_horizon, actuals,
    type = "b", pch = 16,
    xlab = "Dias", ylab = "Valor",
    main = paste("MLP Tunado –", col),
    col  = "red", ylim = range(c(actuals, preds), na.rm = TRUE)
  )
  lines(
    1:forecast_horizon, preds,
    type = "b", pch = 17, col = "darkgreen"
  )
  legend(
    "topright",
    legend = c("Real", "MLP"),
    col    = c("red", "darkgreen"),
    pch    = c(16, 17)
  )
}
