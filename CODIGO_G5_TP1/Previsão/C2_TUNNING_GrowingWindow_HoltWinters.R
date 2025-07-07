library(forecast)
library(rminer)

# 1. Parâmetros base
forecast_horizon <- 7
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- head(sales, -3)
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# 2. Grelha de procura (valores de HW + janelas)
grid <- expand.grid(
  alpha      = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  beta       = c(FALSE),
  gamma      = c(TRUE),
  lag_window = c(14, 15, 30),
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
    alpha <- params$alpha
    beta <- params$beta
    gamma <- params$gamma
    
    W <- 365 - forecast_horizon - (Runs - 1) * increment
    if (W < 2 * 7) next  # Verifica se há dados suficientes (mínimo 2 ciclos)
    
    YR <- diff(range(d1))
    nmae_v <- numeric(Runs)
    
    for (b in 1:Runs) {
      HLD <- holdout(d1, ratio=forecast_horizon, mode="incremental", iter=b, window=lag_window, increment=increment)
      dtr <- ts(d1[HLD$tr], frequency=7)
      
      if (length(dtr) < 2 * 7) next  # Verifica se a janela é suficiente para HW
      
      M <- tryCatch({
        HoltWinters(dtr, alpha=alpha, beta=beta, gamma=gamma)
      }, error = function(e) return(NULL))
      
      if (is.null(M)) {
        nmae_v[b] <- NA
        next
      }
      
      P <- tryCatch({
        forecast(M, h=length(HLD$ts))$mean[1:forecast_horizon]
      }, error = function(e) return(rep(NA, forecast_horizon)))
      
      A <- d1[HLD$ts]
      
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
    
    cat(sprintf("Testado: α=%.1f, β=%s, γ=%s, lag=%d, inc=%d, runs=%d → NMAE=%.3f\n",
                alpha, beta, gamma, lag_window, increment, Runs, avg_nmae))
    
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
    
    # Gráfico da melhor previsão
    mgraph(
      melhor_actuals, melhor_preds,
      graph="REG", Grid=10, col=c("black", "blue"),
      leg=list(pos="topright", leg=c("real", "previsto")),
      main=paste("Melhor Growing Holt-Winters –", col)
    )
  } else {
    cat("⚠️ Nenhuma configuração válida encontrada para", col, "\n")
  }
}
