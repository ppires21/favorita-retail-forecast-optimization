# C2_GrowingWindow_XGBOOST.R
library(rminer)

# 1. Parâmetros fixos
forecast_horizon <- 7    # horizonte de previsão de 7 dias
lag_window       <- 15   # número de lags para as features
increment        <- 7    # incremento de 7 dias a cada iteração
Runs             <- 10   # número de iterações (semanas de teste)

# 2. Carregamento dos dados
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- sales[1:(nrow(sales) - 3), ]
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# 3. Hiperparâmetros XGBoost por série
params_xgb <- list(
  s_d11 = list(eta=0.05, max_depth=6, nrounds=50),
  s_d12 = list(eta=0.05, max_depth=4, nrounds=150),
  s_e11 = list(eta=0.20, max_depth=6, nrounds=150),
  s_e12 = list(eta=0.05, max_depth=6, nrounds=50),
  s_b11 = list(eta=0.05, max_depth=5, nrounds=100),
  s_b12 = list(eta=0.20, max_depth=6, nrounds=50)
)

# 4. Loop por cada série
for (col in sales_cols) {
  cat("\n=== Growing Window – Série:", col, "===\n")
  
  # 4.1. Extrair últimos 365 dias
  serie_total <- sales_cleaned[[col]]
  serie <- tail(serie_total, 365)
  d1 <- as.numeric(serie)
  
  # 4.2. Calcular janelas
  W  <- 365 - forecast_horizon - (Runs - 1) * increment
  W2 <- W - lag_window
  
  # 4.3. Criar matriz de lags
  D <- CasesSeries(d1, 1:lag_window)
  YR <- diff(range(d1))
  
  # 4.4. Vetores para métricas
  mae_v  <- numeric(Runs)
  nmae_v <- numeric(Runs)
  rmse_v <- numeric(Runs)
  rrse_v <- numeric(Runs)
  
  # imprime parâmetros
  cat("Parâmetros XGBoost:\n")
  print(params_xgb[[col]])
  
  # 4.5. Growing window – múltiplas previsões
  for (b in 1:Runs) {
    H1 <- holdout(d1,
                  ratio     = forecast_horizon,
                  mode      = "incremental",
                  iter      = b,
                  window    = W,
                  increment = increment)
    H2 <- holdout(D$y,
                  ratio     = forecast_horizon,
                  mode      = "incremental",
                  iter      = b,
                  window    = W2,
                  increment = increment)
    
    p <- params_xgb[[col]]
    M <- fit(y ~ ., D[H2$tr, ], model = "xgboost",
             eta       = p$eta,
             max_depth = p$max_depth,
             nrounds   = p$nrounds,
             objective = "reg:squarederror")
    
    P <- lforecast(M, D,
                   start   = length(H2$tr) + 1,
                   horizon = forecast_horizon)
    A <- d1[H1$ts]
    
    mae_v[b]  <- mmetric(A, P, metric="MAE")
    nmae_v[b] <- mmetric(A, P, metric="NMAE", val=YR)
    rmse_v[b] <- mmetric(A, P, metric="RMSE")
    rrse_v[b] <- mmetric(A, P, metric="RRSE")
    
    cat(sprintf("Iter %2d – MAE=%.3f  NMAE=%.3f  RMSE=%.3f  RRSE=%.3f\n",
                b, mae_v[b], nmae_v[b], rmse_v[b], rrse_v[b]))
  }
  
  # 4.6. Estatísticas agregadas
  cat(sprintf("\nMÉDIAS  – MAE=%.3f  NMAE=%.3f  RMSE=%.3f  RRSE=%.3f\n",
              mean(mae_v), mean(nmae_v), mean(rmse_v), mean(rrse_v)))
}