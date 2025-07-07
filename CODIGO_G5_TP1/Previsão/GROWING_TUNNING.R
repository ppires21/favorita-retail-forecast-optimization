library(rminer)

# 1. Parâmetros base
forecast_horizon <- 7
sales <- read.csv("sales.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- head(sales, -3)
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# 2. Fixando a semente para garantir reprodutibilidade
set.seed(123)  # Defina a semente com um número qualquer (pode ser 42 ou outro valor)

# 3. Grelha de procura para MLP (rede neural)
grid <- expand.grid(
  neurons    = c(20, 30, 40),      # Número de neurónios na camada escondida
  iters      = c(200, 300, 400),    # Épocas de treino
  lag_window = c(15),
  increment  = c(7),
  Runs       = c(10)
)

# 4. Tuning por série
for (col in sales_cols) {
  cat(sprintf("\n=== Tuning Growing MLP – Série: %s ===\n", col))
  
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
      
      M <- fit(y~., D[H2$tr,], model="mlp",
               neurons=params$neurons, iters=params$iters)
      
      P <- lforecast(M, D, start=(length(H2$tr)+1), horizon=forecast_horizon)
      A <- d1[H1$ts]
      
      if (any(is.na(P))) {
        nmae_v[b] <- NA
      } else {
        nmae_v[b] <- mmetric(A, P, metric="NMAE", val=YR)
      }
      
      if (b == Runs && !any(is.na(P))) {
        melhor_preds_temp <- P
        melhor_actuals_temp <- A
        melhor_YR_temp <- YR
      }
    }
    
    if (all(is.na(nmae_v))) next
    avg_nmae <- mean(nmae_v, na.rm=TRUE)
    
    cat(sprintf("Testado: neurons=%d, iters=%d, lag=%d, inc=%d, runs=%d → NMAE=%.3f\n",
                params$neurons, params$iters, lag_window, increment, Runs, avg_nmae))
    
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
    
    # Cálculo das métricas adicionais
    mae  <- mmetric(melhor_actuals, melhor_preds, metric="MAE")
    rmse <- mmetric(melhor_actuals, melhor_preds, metric="RMSE")
    rrse <- mmetric(melhor_actuals, melhor_preds, metric="RRSE")
    
    cat(sprintf("→ MAE: %.3f, RMSE: %.3f, RRSE: %.3f\n", mae, rmse, rrse))
    
    # Gráfico final
    mgraph(
      melhor_actuals, melhor_preds,
      graph="REG", Grid=10, col=c("black", "blue"),
      leg=list(pos="topright", leg=c("real", "previsto")),
      main=paste("Melhor Growing MLP –", col)
    )
  } else {
    cat("⚠️ Nenhuma configuração válida encontrada para", col, "\n")
  }
}
