library(rminer)

# 1. Parâmetros base
forecast_horizon <- 7
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- head(sales, -3)
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# 2. Grelha de procura (tudo combinável)
grid <- expand.grid(
  eta        = c(0.05, 0.1, 0.2),
  max_depth  = c(4, 5, 6),
  nrounds    = c(50, 100, 150),
  lag_window = c(7, 15, 30),
  increment  = c(7, 14),
  Runs       = c(10, 20, 30)
)

# 3. Tuning por série
for (col in sales_cols) {
  cat(sprintf("\n=== Tuning Growing – Série: %s ===\n", col))
  
  serie_total <- sales_cleaned[[col]]
  serie <- tail(serie_total, 365)
  d1 <- as.numeric(serie)
  
  melhor_nmae <- Inf
  melhor_cfg <- NULL
  melhor_preds <- NULL
  melhor_actuals <- NULL
  melhor_YR <- NULL
  
  for (i in 1:nrow(grid)) {
    params <- grid[i, ]
    
    lag_window <- params$lag_window
    increment <- params$increment
    Runs <- params$Runs
    timelags <- 1:lag_window
    
    W <- 365 - forecast_horizon - (Runs - 1) * increment
    W2 <- W - lag_window
    if (W <= 0 || W2 <= 0) next
    
    D <- CasesSeries(d1, timelags)
    YR <- diff(range(d1))
    nmae_v <- numeric(Runs)
    
    for (b in 1:Runs) {
      H1 <- holdout(d1, ratio=forecast_horizon, mode="incremental", iter=b, window=W, increment=increment)
      H2 <- holdout(D$y, ratio=forecast_horizon, mode="incremental", iter=b, window=W2, increment=increment)
      
      M <- fit(y~., D[H2$tr,], model="xgboost",
               eta=params$eta, max_depth=params$max_depth, nrounds=params$nrounds,
               objective="reg:squarederror")
      
      P <- lforecast(M, D, start=(length(H2$tr)+1), horizon=forecast_horizon)
      A <- d1[H1$ts]
      
      if (any(is.na(P))) {
        nmae_v[b] <- NA
      } else {
        nmae_v[b] <- mmetric(A, P, metric="NMAE", val=YR)
      }
      
      # Guardar previsão da última iteração
      if (b == Runs && !any(is.na(P))) {
        melhor_preds_temp <- P
        melhor_actuals_temp <- A
        melhor_YR_temp <- YR
      }
    }
    
    if (all(is.na(nmae_v))) next
    avg_nmae <- mean(nmae_v, na.rm=TRUE)
    
    cat(sprintf("Testado: eta=%.2f, depth=%d, nrounds=%d, lag=%d, inc=%d, runs=%d → NMAE=%.3f\n",
                params$eta, params$max_depth, params$nrounds, lag_window, increment, Runs, avg_nmae))
    
    if (!is.na(avg_nmae) && avg_nmae < melhor_nmae) {
      melhor_nmae <- avg_nmae
      melhor_cfg <- params
      melhor_preds <- melhor_preds_temp
      melhor_actuals <- melhor_actuals_temp
      melhor_YR <- melhor_YR_temp
    }
  }
  
  # Resultado final
  if (!is.null(melhor_cfg)) {
    cat("\nMelhor configuração para", col, ":\n")
    print(melhor_cfg)
    cat(sprintf("→ Melhor NMAE: %.3f\n", melhor_nmae))
    
    # Gráfico da melhor previsão (última janela da melhor config)
    mgraph(
      melhor_actuals, melhor_preds,
      graph="REG", Grid=10, col=c("black", "green"),
      leg=list(pos="topright", leg=c("real", "previsto")),
      main=paste("Melhor Growing XGBoost –", col)
    )
  } else {
    cat("⚠️ Nenhuma configuração válida encontrada para", col, "\n")
  }
}
