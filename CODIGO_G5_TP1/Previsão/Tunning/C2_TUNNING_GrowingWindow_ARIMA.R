library(rminer)
library(forecast)

# 1. Parâmetros base
forecast_horizon <- 7
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- head(sales, -3)
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# 2. Grelha de avaliação 
grid <- expand.grid(
  increment = c(7, 14),
  Runs      = c(10, 20, 30)
)

# 3. Tuning por série (Growing Window)
for (col in sales_cols) {
  cat(sprintf("\n=== Tuning Growing ARIMA – Série: %s ===\n", col))
  serie <- tail(sales_cleaned[[col]], 365)
  YR <- diff(range(serie))
  
  best_nmae  <- Inf
  best_cfg   <- NULL
  best_preds <- NULL
  best_acts  <- NULL
  
  for (i in seq_len(nrow(grid))) {
    inc  <- grid$increment[i]
    Runs <- grid$Runs[i]
    # tamanho inicial da janela de treino
    W <- length(serie) - forecast_horizon - (Runs - 1) * inc
    if (W <= 0) next
    
    nmae_vals <- numeric(Runs)
    lastP <- lastA <- NULL
    
    for (b in 1:Runs) {
      H <- holdout(serie, ratio = forecast_horizon,
                   mode = "incremental", iter = b,
                   window = W, increment = inc)
      tr_idx <- H$tr
      ts_idx <- H$ts
      train_ts <- ts(serie[tr_idx], frequency = 1)
      
      # ajuste automático ARIMA
      fit <- auto.arima(train_ts, stepwise = FALSE, approximation = FALSE)
      fc  <- forecast(fit, h = forecast_horizon)
      P   <- as.numeric(fc$mean)
      A   <- serie[ts_idx]
      
      nmae_vals[b] <- if (anyNA(P)) NA else mmetric(A, P, metric = "NMAE", val = YR)
      if (b == Runs && !anyNA(P)) {
        lastP <- P
        lastA <- A
      }
    }
    
    avg_nmae <- mean(nmae_vals, na.rm = TRUE)
    cat(sprintf("Testado: inc=%d, runs=%d → NMAE=%.3f\n", inc, Runs, avg_nmae))
    
    if (!is.na(avg_nmae) && avg_nmae < best_nmae) {
      best_nmae  <- avg_nmae
      best_cfg   <- grid[i, ]
      best_preds <- lastP
      best_acts  <- lastA
    }
  }
  
  if (is.null(best_cfg)) {
    cat(sprintf("⚠️ Nenhuma configuração válida para %s\n", col))
    next
  }
  
  cat(sprintf("\nMelhor configuração para %s:\n", col))
  print(best_cfg)
  cat(sprintf("→ Melhor NMAE: %.3f\n", best_nmae))
  
  # Gráfico da melhor previsão
  mgraph(
    best_acts, best_preds,
    graph = "REG", Grid = 10,
    col = c("black", "blue"),
    leg = list(pos = "topright", leg = c("real", "previsto")),
    main = paste("ARIMA Growing –", col)
  )
}