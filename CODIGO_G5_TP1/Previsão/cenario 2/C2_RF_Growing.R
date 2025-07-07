######################################
# Growing Window – Random Forest
######################################

library(rminer)
library(randomForest)

# 1. Parâmetros globais fixos
forecast_horizon <- 7
lag_window       <- 15
increment        <- 7
Runs             <- 10
ntree            <- 100
mtry             <- 6

# 2. Carregamento dos dados
sales <- read.csv("C:/Users/joaoa/OneDrive/Documentos/Rstudio/sales_clean.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- head(sales, -3)
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# 3. Para salvar último A e P de cada série
resultados <- list()

# 4. Loop de Growing
for (col in sales_cols) {
  cat("\n=== Growing Window – Série:", col, "===\n")
  serie_total <- sales_cleaned[[col]]
  serie        <- tail(serie_total, 365)
  d1           <- as.numeric(serie)
  
  timelags    <- 1:lag_window
  W           <- 365 - forecast_horizon - (Runs - 1) * increment
  W2          <- W - lag_window
  
  D    <- CasesSeries(d1, timelags)
  YR   <- diff(range(d1))
  mae_v   <- nmae_v <- rmse_v <- rrse_v <- numeric(Runs)
  
  for (b in 1:Runs) {
    H1 <- holdout(d1, ratio=forecast_horizon, mode="incremental",
                  iter=b, window=W, increment=increment)
    H2 <- holdout(D$y, ratio=forecast_horizon, mode="incremental",
                  iter=b, window=W2, increment=increment)
    
    M  <- fit(y ~ ., D[H2$tr, ], model="randomForest",
              ntree=ntree, mtry=mtry)
    
    P  <- lforecast(M, D, start=(length(H2$tr)+1), horizon=forecast_horizon)
    A  <- d1[H1$ts]
    
    mae_v[b]  <- mmetric(A, P, "MAE")
    nmae_v[b] <- mmetric(A, P, "NMAE", val=YR)
    rmse_v[b] <- mmetric(A, P, "RMSE")
    rrse_v[b] <- mmetric(A, P, "RRSE")
    
    cat(sprintf("Iter %2d → MAE=%.3f  NMAE=%.3f  RMSE=%.3f  RRSE=%.3f\n",
                b, mae_v[b], nmae_v[b], rmse_v[b], rrse_v[b]))
  }
  
  cat(sprintf("MÉDIAS     → MAE=%.3f  NMAE=%.3f  RMSE=%.3f  RRSE=%.3f\n",
              mean(mae_v), mean(nmae_v), mean(rmse_v), mean(rrse_v)))
  
  # Guarda último A e P para plotar depois
  resultados[[col]] <- list(A = A, P = P)
}

# 5. Gráficos em grelha
par(mfrow = c(2, 3))  # 2 linhas, 3 colunas
for (col in sales_cols) {
  A <- resultados[[col]]$A
  P <- resultados[[col]]$P
  
  mgraph(A, P, graph="REG", Grid=10,
         col=c("black", "green"),
         leg=list(pos="topright", leg=c("real", "previsto")),
         main=paste("Growing RF –", col))
}
