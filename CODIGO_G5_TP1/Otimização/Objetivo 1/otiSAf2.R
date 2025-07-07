# simulated_annealing_fase2.R

# 0) Pacotes e fontes
library(rminer)
library(forecast)
library(GenSA)

# funções auxiliares: repair(), eval(), calc_upper()
source("otiutils.R")

set.seed(12345)

# 1) Carregamento e limpeza dos dados
sales <- read.csv("sales_clean.csv", sep=";")
sales$date <- as.Date(sales$date)
sales_cleaned <- sales[1:(nrow(sales)-3), ]
sales_cols <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")

# 2) Parâmetros gerais de Growing Window
Runs <- 10            # iterações / semanas de teste
forecast_horizon <- 7
lag_window <- 15
increment <- 7
W  <- 365 - forecast_horizon - (Runs-1)*increment
W2 <- W - lag_window

# hiperparâmetros XGBoost (igual ao Monte Carlo)
params_xgb <- list(
  s_d11 = list(eta=0.05, max_depth=6, nrounds=50),
  s_d12 = list(eta=0.05, max_depth=4, nrounds=150),
  s_e11 = list(eta=0.20, max_depth=6, nrounds=150),
  s_e12 = list(eta=0.05, max_depth=6, nrounds=50),
  s_b11 = NULL,  # ARIMA
  s_b12 = list(eta=0.20, max_depth=6, nrounds=50)
)

# 3) Função de forecasting por série e iteração
forecast_series <- function(col, b) {
  serie_total <- sales_cleaned[[col]]
  serie <- tail(serie_total, 365)
  d1 <- as.numeric(serie)
  
  H1 <- holdout(d1,
                ratio = forecast_horizon,
                mode = "incremental",
                iter = b,
                window = W,
                increment = increment)
  
  if (col == "s_b11") {
    ts_train <- ts(d1[H1$tr], frequency = 7)
    fit_arima <- auto.arima(ts_train)
    P <- as.numeric(forecast(fit_arima, h=forecast_horizon)$mean)
  } else {
    p <- params_xgb[[col]]
    Dset <- CasesSeries(d1, 1:lag_window)
    H2 <- holdout(Dset$y,
                  ratio = forecast_horizon,
                  mode = "incremental",
                  iter = b,
                  window = W2,
                  increment = increment)
    model <- fit(y ~ ., Dset[H2$tr,],
                 model = "xgboost",
                 eta = p$eta,
                 max_depth = p$max_depth,
                 nrounds = p$nrounds,
                 objective = "reg:squarederror")
    P <- lforecast(model, Dset,
                   start = length(H2$tr) + 1,
                   horizon = forecast_horizon)
  }
  
  return(P)
}

# 4) Gera lista de matrizes 7×6 de previsões
pred_list <- lapply(1:Runs, function(b) {
  mat <- matrix(NA, nrow=forecast_horizon, ncol=length(sales_cols),
                dimnames=list(NULL, sales_cols))
  for (j in seq_along(sales_cols))
    mat[, j] <- forecast_series(sales_cols[j], b)
  mat
})

# 5) Função de otimização por Simulated Annealing (GenSA)
optimiza_semana_SA <- function(pred_matrix, max.call=5000) {
  assign("pred_sales", pred_matrix, envir = .GlobalEnv)
  
  upper <- calc_upper(pred_sales)
  n_vars <- length(upper)
  lower <- rep(0, n_vars)
  
  profit_history <- c()
  
  fn_sa <- function(s) {
    s <- pmin(pmax(round(s), lower), upper)
    s <- repair(s)
    profit <- eval(s)
    
    if (is.finite(profit)) {
      profit_history <<- c(profit_history, profit)
    }
    
    if (profit < 0) return(1e6)
    return(-profit)
  }
  
  res <- GenSA(
    par = rep(1, n_vars),
    lower = lower,
    upper = upper,
    fn = fn_sa,
    control = list(max.call = max.call, verbose = FALSE)
  )
  
  best_mem <- round(res$par)
  best_mem <- pmin(pmax(best_mem, lower), upper)
  best_rep <- repair(best_mem)
  lucro <- eval(best_rep)
  
  list(lucro = lucro, sol = best_rep, profit_history = profit_history)
}

# 6) Loop sobre as semanas + agregação de resultados
run_phase2_SA <- function(pred_list, max.call=5000, aggr=c("mean","median")) {
  aggr <- match.arg(aggr)
  lucros <- numeric(length(pred_list))
  sols <- vector("list", length(pred_list))
  profits_histories <- vector("list", length(pred_list))
  
  for (i in seq_along(pred_list)) {
    cat(sprintf("Semana %2d/%d — ", i, length(pred_list)))
    out <- optimiza_semana_SA(pred_list[[i]], max.call = max.call)
    cat(sprintf("Lucro = %.2f EUR\n", out$lucro))
    lucros[i] <- out$lucro
    sols[[i]] <- out$sol
    profits_histories[[i]] <- out$profit_history
  }
  
  lucro_agr <- if (aggr == "mean") mean(lucros) else median(lucros)
  cat("============================================\n")
  cat(sprintf("%s dos lucros = %.2f EUR\n", toupper(aggr), lucro_agr))
  
  list(lucros = lucros, lucro_agr = lucro_agr, sols = sols, profits_histories = profits_histories)
}

# 7) Executa e gera resultados
resultados_SA <- run_phase2_SA(pred_list, max.call=5000, aggr="median")

# 8) Monta data.frame e exibe
df_res_SA <- data.frame(
  Metodo = "SimulatedAnnealing",
  Semana = seq_along(resultados_SA$lucros),
  Lucro = resultados_SA$lucros,
  LucroAgregado = resultados_SA$lucro_agr
)

cat("\n--- Tabela de Lucros por Semana (SA) ---\n")
print(df_res_SA)
cat(sprintf("\nLucro agregado (mediana) = %.2f EUR\n\n", resultados_SA$lucro_agr))

# 9) Gráfico da convergência por semana
colors <- rainbow(length(resultados_SA$profits_histories))

plot(NULL,
     xlim = c(1, max(sapply(resultados_SA$profits_histories, length))),
     ylim = range(unlist(resultados_SA$profits_histories), na.rm = TRUE),
     xlab = "Avaliações da função objetivo",
     ylab = "Lucro",
     main = "Convergência do lucro por semana - Simulated Annealing")

for (i in seq_along(resultados_SA$profits_histories)) {
  lines(resultados_SA$profits_histories[[i]], col = colors[i], lwd = 2)
}

legend("bottomright",
       legend = paste("Semana", seq_along(resultados_SA$profits_histories)),
       col = colors, lwd = 2, cex = 0.8)

grid()
