# 1.CENARIO_MULTIVARIADO_ARIMAX.R: previsão multivariada multi-step usando ARIMAX individual por série

library(rminer)
library(forecast)

# --- 1. Definição de Parâmetros ---
series_cols <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")
horizon     <- 7  # passos à frente para previsão

# --- 2. Carregamento dos dados ---
sales <- read.csv("sales_clean.csv", sep=";")
sales$date <- as.Date(sales$date)
cdata <- as.matrix(sales[, series_cols])
# holdout fixo baseado na 1ª série
hd <- holdout(cdata[,1], ratio=horizon, mode="order")
# dados de treino e teste
train_mat <- cdata[hd$tr, ]
test_mat  <- cdata[hd$ts, ]

# --- 3. Ajustes e previsão ARIMAX para cada série ---
pred_list <- vector("list", length(series_cols))
names(pred_list) <- series_cols
metrics <- data.frame(Serie=series_cols, MAE=NA, NMAE=NA, RMSE=NA, RRSE=NA)

for (i in seq_along(series_cols)) {
  col_name <- series_cols[i]
  y_train <- train_mat[, i]
  y_test  <- test_mat[, i]
  # exógenas: todas as outras séries
  if (ncol(train_mat) > 1) {
    xreg_train <- train_mat[, -i, drop=FALSE]
    xreg_test  <- test_mat[, -i, drop=FALSE]
  } else {
    xreg_train <- NULL
    xreg_test  <- NULL
  }
  ts_train <- ts(y_train, frequency = 1)
  
  # auto.arima com exógenas
  if (!is.null(xreg_train)) {
    fit <- auto.arima(ts_train, xreg = xreg_train)
  } else {
    fit <- auto.arima(ts_train)
  }
  ord <- arimaorder(fit)
  cat(sprintf("Modelo ARIMAX para %s: arima(%d,%d,%d)\n", 
              col_name, ord["p"], ord["d"], ord["q"]))
  
  # previsão com exógenas futuras
  if (!is.null(xreg_test)) {
    fc <- forecast(fit, h = horizon, xreg = xreg_test)
  } else {
    fc <- forecast(fit, h = horizon)
  }
  preds <- as.numeric(fc$mean)
  pred_list[[i]] <- preds
  
  # métricas
  srange <- diff(range(y_test, na.rm = TRUE))
  mae   <- mmetric(y_test, preds, metric="MAE")
  nmae  <- mmetric(y_test, preds, metric="NMAE", val = srange)
  rmse  <- mmetric(y_test, preds, metric="RMSE")
  rrse  <- mmetric(y_test, preds, metric="RRSE")
  metrics[i, 2:5] <- c(mae, nmae, rmse, rrse)
  
  # plot individual
  plot(1:horizon, y_test, type="b", pch=16, col="black",
       xlab="Passo à frente", ylab="Valor",
       main=paste(col_name, "- ARIMAX"),
       ylim=range(c(y_test, preds), na.rm=TRUE))
  lines(1:horizon, preds, type="b", pch=17, col="blue")
  legend("topleft", legend=c("Real","Previsto"), col=c("black","blue"), pch=c(16,17))
}

# --- 4. Resultados agregados ---
print(metrics)
