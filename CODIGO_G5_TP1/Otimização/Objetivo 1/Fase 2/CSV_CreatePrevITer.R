# ficheiro: CSV_CreatePrevIter.R

# 0) Pacotes
library(rminer)
library(forecast)


# 1) Carregamento e limpeza dos dados originais
sales <- read.csv("sales_clean.csv", sep=";")
sales$date <- as.Date(sales$date)
sales_cleaned <- sales[1:(nrow(sales)-3), ]
sales_cols    <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")

# 2) Parâmetros do Growing Window
Runs             <- 10      # número de iterações / semanas de teste
forecast_horizon <- 7       # horizonte de 7 dias
lag_window       <- 15      # tamanho da janela de lags para XGBoost
increment        <- 7       # incremento por iteração
W  <- 365 - forecast_horizon - (Runs-1)*increment
W2 <- W - lag_window

# 3) Hiperparâmetros XGBoost por série (s_b11 faz ARIMA)
params_xgb <- list(
  s_d11 = list(eta=0.05, max_depth=6, nrounds=50),
  s_d12 = list(eta=0.05, max_depth=4, nrounds=150),
  s_e11 = list(eta=0.20, max_depth=6, nrounds=150),
  s_e12 = list(eta=0.05, max_depth=6, nrounds=50),
  s_b11 = NULL,
  s_b12 = list(eta=0.20, max_depth=6, nrounds=50)
)

# 4) Função de forecasting para cada série e iteração
forecast_series <- function(col, b) {
  serie_total <- sales_cleaned[[col]]
  serie       <- tail(serie_total, 365)
  d1          <- as.numeric(serie)
  
  H1 <- holdout(d1,
                ratio     = forecast_horizon,
                mode      = "incremental",
                iter      = b,
                window    = W,
                increment = increment)
  
  if (col == "s_b11") {
    ts_train  <- ts(d1[H1$tr], frequency = 7)
    fit_arima <- auto.arima(ts_train)
    fc        <- forecast(fit_arima, h = forecast_horizon)
    P         <- as.numeric(fc$mean)
  } else {
    p    <- params_xgb[[col]]
    Dset <- CasesSeries(d1, 1:lag_window)
    
    H2 <- holdout(Dset$y,
                  ratio     = forecast_horizon,
                  mode      = "incremental",
                  iter      = b,
                  window    = W2,
                  increment = increment)
    
    model <- fit(y ~ ., Dset[H2$tr,],
                 model     = "xgboost",
                 eta       = p$eta,
                 max_depth = p$max_depth,
                 nrounds   = p$nrounds,
                 objective = "reg:squarederror")
    
    P <- lforecast(model, Dset,
                   start   = length(H2$tr) + 1,
                   horizon = forecast_horizon)
  }
  return(P)
}

# 5) Gera lista de previsões
pred_list <- vector("list", Runs)
for (b in 1:Runs) {
  mat <- matrix(NA,
                nrow = forecast_horizon,
                ncol = length(sales_cols),
                dimnames = list(NULL, sales_cols))
  for (j in seq_along(sales_cols)) {
    mat[, j] <- forecast_series(sales_cols[j], b)
  }
  pred_list[[b]] <- mat
}

# 6) Monta data.frame e exporta para CSV
df_pred <- data.frame(
  Iteracao = rep(1:Runs, each = forecast_horizon),
  Dia      = rep(1:forecast_horizon, times = Runs)
)
for (j in seq_along(sales_cols)) {
  nome <- sales_cols[j]
  df_pred[[nome]] <- unlist(lapply(pred_list, function(M) M[, nome]))
}

write.csv(df_pred,
          file      = "ITER_Sales_prev.csv",
          row.names = FALSE)

cat("Previsões gravadas em 'ITER_Sales_prev.csv'\n")