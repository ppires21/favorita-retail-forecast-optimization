
# --- Script para gerar previsões e guardar em CSV ---
library(rminer)
library(forecast)

set.seed(12345)

# Carregamento dos dados
sales <- read.csv("sales_clean.csv", sep=";")
sales$date <- as.Date(sales$date)
sales_cleaned <- sales[1:(nrow(sales)-3), ]
sales_cols <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")

# Parâmetros gerais
Runs <- 10
forecast_horizon <- 7
lag_window <- 15
increment <- 7
W <- 365 - forecast_horizon - (Runs-1)*increment
W2 <- W - lag_window

# Hiperparâmetros
params_xgb <- list(
  s_d11 = list(eta=0.05, max_depth=6, nrounds=50),
  s_d12 = list(eta=0.05, max_depth=4, nrounds=150),
  s_e11 = list(eta=0.20, max_depth=6, nrounds=150),
  s_e12 = list(eta=0.05, max_depth=6, nrounds=50),
  s_b11 = NULL,
  s_b12 = list(eta=0.20, max_depth=6, nrounds=50)
)

# Função de previsão
forecast_series <- function(col, b) {
  serie_total <- sales_cleaned[[col]]
  serie <- tail(serie_total, 365)
  d1 <- as.numeric(serie)
  H1 <- holdout(d1, ratio=forecast_horizon, mode="incremental", iter=b, window=W, increment=increment)
  
  if (col == "s_b11") {
    ts_train <- ts(d1[H1$tr], frequency=7)
    fit_arima <- auto.arima(ts_train)
    P <- as.numeric(forecast(fit_arima, h=forecast_horizon)$mean)
  } else {
    p <- params_xgb[[col]]
    Dset <- CasesSeries(d1, 1:lag_window)
    H2 <- holdout(Dset$y, ratio=forecast_horizon, mode="incremental", iter=b, window=W2, increment=increment)
    model <- fit(y ~ ., Dset[H2$tr,], model="xgboost", eta=p$eta, max_depth=p$max_depth, nrounds=p$nrounds, objective="reg:squarederror")
    P <- lforecast(model, Dset, start=length(H2$tr) + 1, horizon=forecast_horizon)
  }
  return(P)
}

# Gerar lista de previsões
pred_list <- vector("list", Runs)
for (b in 1:Runs) {
  mat <- matrix(NA, nrow=forecast_horizon, ncol=length(sales_cols), dimnames=list(NULL, sales_cols))
  for (j in seq_along(sales_cols)) {
    mat[, j] <- forecast_series(sales_cols[j], b)
  }
  pred_list[[b]] <- mat
}

# Combinar num único data.frame
df_all <- do.call(cbind, lapply(seq_along(pred_list), function(i) {
  df <- as.data.frame(pred_list[[i]])
  colnames(df) <- paste0(colnames(df), "_", i)
  return(df)
}))

# Guardar num ficheiro CSV
write.csv(df_all, "previsoes_10_iteracoes.csv", row.names = FALSE)
