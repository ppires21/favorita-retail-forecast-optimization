library(rminer)

# 1. Parâmetros fixos
forecast_horizon <- 7
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- head(sales, -3)
sales_cols <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")

# 2. Grelha combinada de parâmetros a testar
grid <- expand.grid(
  eta        = c(0.05, 0.1, 0.2),
  max_depth  = c(4, 5, 6),
  nrounds    = c(50, 100, 150),
  lag_window = c(7, 15, 30),
  increment  = c(7, 14),
  Runs       = c(10, 20, 30)
)

# 3. Loop por cada série
for (col in sales_cols) {
  cat("\n=== Tuning Rolling – Série:", col, "===\n")
  serie_total <- sales_cleaned[[col]]
  serie <- tail(serie_total, 365)
  d1 <- as.numeric(serie)
  
  melhor_nmae   <- Inf
  melhor_config <- NULL
  melhor_preds  <- NULL
  melhor_actuals <- NULL
  
  # 4. Tuning grid search
  for (i in 1:nrow(grid)) {
    params     <- grid[i, ]
    lag_window <- params$lag_window
    timelags   <- 1:lag_window
    increment  <- params$increment
    Runs       <- params$Runs
    
    # Recalcular tamanhos de janela
    W  <- 365 - forecast_horizon - (Runs - 1) * increment
    W2 <- W - lag_window
    if (W2 <= 0) next  # ignora configurações inválidas
    
    D <- CasesSeries(d1, timelags)
    YR <- diff(range(d1))
    
    nmae_total <- c()
    
    for (b in 1:Runs) {
      H1 <- holdout(d1, ratio=forecast_horizon, mode="rolling", iter=b, window=W, increment=increment)
      H2 <- holdout(D$y, ratio=forecast_horizon, mode="rolling", iter=b, window=W2, increment=increment)
      
      M <- fit(y~., D[H2$tr,], model="xgboost",
               eta=params$eta, max_depth=params$max_depth, nrounds=params$nrounds,
               objective="reg:squarederror")
      
      P <- lforecast(M, D, start=(length(H2$tr)+1), horizon=forecast_horizon)
      A <- d1[H1$ts]
      
      if (any(is.na(P))) next
      nmae_val <- mmetric(A, P, metric="NMAE", val=YR)
      nmae_total <- c(nmae_total, nmae_val)
    }
    
    nmae_medio <- mean(nmae_total)
    
    cat(sprintf("Testado: eta=%.2f, depth=%d, nrounds=%d, lag=%d, inc=%d, runs=%d → NMAE=%.3f\n",
                params$eta, params$max_depth, params$nrounds,
                lag_window, increment, Runs, nmae_medio))
    
    if (!is.na(nmae_medio) && nmae_medio < melhor_nmae) {
      melhor_nmae   <- nmae_medio
      melhor_config <- params
      melhor_preds  <- P
      melhor_actuals <- A
    }
  }
  
  # 5. Mostrar melhor configuração
  if (is.null(melhor_config)) {
    cat("⚠️ Nenhuma configuração válida para", col, "\n")
    next
  }
  
  cat("\nMelhor configuração para", col, ":\n")
  print(melhor_config)
  cat(sprintf("→ Melhor NMAE: %.3f\n", melhor_nmae))
  
  # 6. Gráfico da última previsão
  plot(
    1:forecast_horizon, melhor_actuals,
    type = "b", pch = 16,
    xlab = "Dias", ylab = "Valor",
    main = paste("Melhor Rolling –", col),
    col = "red", ylim = range(c(melhor_actuals, melhor_preds), na.rm = TRUE)
  )
  lines(1:forecast_horizon, melhor_preds, type = "b", pch = 17, col = "darkgreen")
  legend("topright", legend = c("Real", "Previsto"), col = c("red", "darkgreen"), pch = c(16, 17))
}
