library(rminer)

# 1. Parâmetros fixos
forecast_horizon <- 7
sales <- read.csv("C:/Users/joaoa/OneDrive/Documentos/Rstudio/sales_clean.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- head(sales, -3)
sales_cols <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")

# 2. Grelha combinada de parâmetros a testar (RF)
grid <- expand.grid(
  lag_window = c(7, 15, 30),
  increment  = c(7, 14),
  Runs       = c(10, 20, 30),
  ntree      = c(50, 100, 200),
  mtry       = c(2, 4, 6)
)

# 3. Loop por cada série
for (col in sales_cols) {
  cat("\n=== Tuning Growing RF – Série:", col, "===\n")
  serie_total <- sales_cleaned[[col]]
  serie <- tail(serie_total, 365)
  d1 <- as.numeric(serie)
  
  melhor_nmae    <- Inf
  melhor_config  <- NULL
  melhor_preds   <- NULL
  melhor_actuals <- NULL
  
  # 4. Tuning grid search
  for (i in seq_len(nrow(grid))) {
    params     <- grid[i, ]
    lag_window <- params$lag_window
    timelags   <- 1:lag_window
    increment  <- params$increment
    Runs       <- params$Runs
    
    W  <- length(d1) - forecast_horizon - (Runs - 1) * increment
    W2 <- W - lag_window
    if (W2 <= 0) next
    
    D  <- CasesSeries(d1, timelags)
    YR <- diff(range(d1))
    nmaes <- c()
    
    for (b in 1:Runs) {
      H1 <- holdout(d1, ratio=forecast_horizon,
                    mode="growing", iter=b,
                    window=W, increment=increment)
      H2 <- holdout(D$y, ratio=forecast_horizon,
                    mode="growing", iter=b,
                    window=W2, increment=increment)
      
      M <- fit(y~., D[H2$tr, ], model="randomForest",
               ntree = params$ntree,
               mtry  = params$mtry)
      
      P <- lforecast(M, D,
                     start = length(H2$tr)+1,
                     horizon = forecast_horizon)
      A <- d1[H1$ts]
      
      if (any(is.na(P))) next
      nmaes[b] <- mmetric(A, P, metric="NMAE", val=YR)
    }
    
    if (length(nmaes)==0) next
    nmae_medio <- mean(nmaes, na.rm=TRUE)
    cat(sprintf("Testado: lag=%2d, inc=%2d, runs=%2d, ntree=%4d, mtry=%d → NMAE=%.3f\n",
                lag_window, increment, Runs,
                params$ntree, params$mtry,
                nmae_medio))
    
    if (nmae_medio < melhor_nmae) {
      melhor_nmae    <- nmae_medio
      melhor_config  <- params
      melhor_preds   <- P
      melhor_actuals <- A
    }
  }
  
  if (is.null(melhor_config)) {
    cat("⚠ Nenhuma configuração válida para", col, "\n")
    next
  }
  cat("\nMelhor configuração para", col, ":\n")
  print(melhor_config)
  cat(sprintf("→ Melhor NMAE: %.3f\n\n", melhor_nmae))
  
  plot(
    1:forecast_horizon, melhor_actuals,
    type = "b", pch = 16,
    xlab = "Dias", ylab = "Valor",
    main = paste("RF Growing –", col),
    col = "red",
    ylim = range(c(melhor_actuals, melhor_preds), na.rm = TRUE)
  )
  lines(
    1:forecast_horizon, melhor_preds,
    type = "b", pch = 17, col = "darkgreen"
  )
  legend(
    "topright",
    legend = c("Real", "Previsto"),
    col    = c("red", "darkgreen"),
    pch    = c(16, 17)
  )
}
