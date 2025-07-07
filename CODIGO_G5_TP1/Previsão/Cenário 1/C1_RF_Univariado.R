library(rminer)
library(randomForest)
source("C:/Users/joaoa/OneDrive/Documentos/Rseries/multi-utils.R")

# --- Parâmetros gerais ---
forecast_horizon <- 7
lag_window       <- 15

# --- Parâmetros RF (fixos para todas as séries) ---
param_rf <- list(ntree = 100, mtry = 6)

# --- Carregamento dos dados ---
sales <- read.csv("C:/Users/joaoa/OneDrive/Documentos/Rstudio/sales_clean.csv", sep = ";")
sales$date    <- as.Date(sales$date)
sales_cleaned <- head(sales, -3)
sales_cols    <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# --- Para salvar resultados ---
resultados <- list()

# --- Loop principal para cálculo ---
for (col in sales_cols) {
  cat("\n=== Coluna:", col, "===\n")
  
  serie     <- sales_cleaned[[col]]
  train_ser <- head(serie, -forecast_horizon)
  serie_ext <- c(train_ser, rep(NA, forecast_horizon))
  
  D_ext     <- CasesSeries(serie_ext, 1:lag_window)
  train_idx <- which(!is.na(D_ext$y))
  start_idx <- max(train_idx) + 1
  actuals   <- tail(serie, forecast_horizon)
  
  modelo_rf <- fit(
    y ~ ., data   = D_ext[train_idx, ],
    model       = "randomForest",
    ntree       = param_rf$ntree,
    mtry        = param_rf$mtry
  )
  
  preds <- lforecast(modelo_rf, D_ext, start = start_idx, horizon = forecast_horizon)
  
  # Avaliação
  srange <- diff(range(actuals, na.rm = TRUE))
  mae    <- mmetric(actuals, preds, metric = "MAE")
  nmae   <- mmetric(actuals, preds, metric = "NMAE", val = srange)
  rmse   <- mmetric(actuals, preds, metric = "RMSE")
  rrse   <- mmetric(actuals, preds, metric = "RRSE")
  
  cat("\nParâmetros usados:\n")
  print(param_rf)
  
  cat("\nMétricas:\n")
  cat(sprintf(" MAE   = %.3f\n", mae))
  cat(sprintf(" NMAE  = %.3f\n", nmae))
  cat(sprintf(" RMSE  = %.3f\n", rmse))
  cat(sprintf(" RRSE  = %.3f\n\n", rrse))
  
  # Guardar resultados
  resultados[[col]] <- list(
    actuals = actuals,
    preds   = preds
  )
}

# --- Depois: fazer todos os gráficos juntos ---
par(mfrow = c(2, 3))  # 2 linhas, 3 colunas

for (col in sales_cols) {
  actuals <- resultados[[col]]$actuals
  preds   <- resultados[[col]]$preds
  
  plot(
    actuals, type = "b", lty = 2, lwd = 2, col = "black", pch = 15,
    xlab = "Examples", ylab = "Values",
    main = paste(col, "- Univariado RF"),
    ylim = range(c(actuals, preds), na.rm = TRUE)
  )
  lines(
    preds, type = "b", lty = 1, lwd = 2, col = "green3", pch = 16
  )
  
  legend(
    "topright",
    legend = c("target", "previsto"),
    col    = c("black", "green3"),
    lty    = c(2, 1),
    lwd    = 2,
    pch    = c(15, 16),
    bty    = "n"
  )
}
