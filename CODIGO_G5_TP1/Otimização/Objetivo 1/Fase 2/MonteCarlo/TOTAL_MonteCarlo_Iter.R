# montecarlo_fase2_completo.R

# 0) Pacotes e fontes
library(rminer)
library(forecast)

source("otiutils.R")    # repair(), eval(), calc_upper()
source("montecarlo.R")  # mcsearch()
source("blind.R")       # fsearch(), dfsearch()

set.seed(12345)

# 1) Carregamento e limpeza dos dados
sales <- read.csv("sales_clean.csv", sep=";")
sales$date <- as.Date(sales$date)
sales_cleaned <- sales[1:(nrow(sales)-3), ]
sales_cols    <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")

# 2) Parâmetros gerais de Growing Window
Runs             <- 10      # número de iterações / semanas de teste
forecast_horizon <- 7       # horizonte de 7 dias
lag_window       <- 15      # tamanho da janela de lags para XGBoost
increment        <- 7       # incremento por iteração
W  <- 365 - forecast_horizon - (Runs-1)*increment
W2 <- W - lag_window

# hiperparâmetros XGBoost por série (exceto s_b11 que é ARIMA)
params_xgb <- list(
  s_d11 = list(eta=0.05, max_depth=6, nrounds=50),
  s_d12 = list(eta=0.05, max_depth=4, nrounds=150),
  s_e11 = list(eta=0.20, max_depth=6, nrounds=150),
  s_e12 = list(eta=0.05, max_depth=6, nrounds=50),
  s_b11 = NULL,
  s_b12 = list(eta=0.20, max_depth=6, nrounds=50)
)

# 3) Função única de forecasting por série e iteração b
forecast_series <- function(col, b) {
  serie_total <- sales_cleaned[[col]]
  serie       <- tail(serie_total, 365)
  d1          <- as.numeric(serie)
  
  # define holdout para treino/teste incremental
  H1 <- holdout(d1,
                ratio     = forecast_horizon,
                mode      = "incremental",
                iter      = b,
                window    = W,
                increment = increment)
  
  if (col == "s_b11") {
    # ==== ARIMA ====
    ts_train   <- ts(d1[H1$tr], frequency = 7)
    fit_arima  <- auto.arima(ts_train)
    fc         <- forecast(fit_arima, h = forecast_horizon)
    P <- as.numeric(fc$mean)
  } else {
    # ==== XGBOOST ====
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

# 4) Constrói lista de matrizes 7×6 (uma por iteração)
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

# 5) Função de otimização Monte Carlo para uma única matriz de previsões
optimiza_semana <- function(pred_matrix, N = 10000) {
  # atribui ao pred_sales global só para o eval()
  assign("pred_sales", pred_matrix, envir = .GlobalEnv)
  
  lower <- rep(0, 70)
  upper <- calc_upper(pred_sales)
  
  res <- mcsearch(
    fn    = function(s) eval(repair(s)),
    lower = lower,
    upper = upper,
    N     = N,
    type  = "max"
  )
  
  list(lucro = res$eval,
       sol   = round(res$sol))
}

# 6) Loop sobre todas as iterações + agregação
run_phase2 <- function(pred_list, N = 10000, aggr = c("mean", "median")) {
  aggr   <- match.arg(aggr)
  lucros <- numeric(length(pred_list))
  sols   <- vector("list", length(pred_list))
  
  for (i in seq_along(pred_list)) {
    cat(sprintf("Semana %2d/%d — ", i, length(pred_list)))
    out <- optimiza_semana(pred_list[[i]], N = N)
    cat(sprintf("Lucro = %.2f EUR\n", out$lucro))
    lucros[i] <- out$lucro
    sols[[i]] <- out$sol
  }
  
  lucro_agr <- if (aggr == "mean") mean(lucros) else median(lucros)
  cat("============================================\n")
  cat(sprintf("%s dos lucros = %.2f EUR\n", toupper(aggr), lucro_agr))
  
  list(lucros    = lucros,
       lucro_agr = lucro_agr,
       sols      = sols)
}

# 7) Executa tudo e monta data.frame de resultados
resultados <- run_phase2(pred_list, N = 10000, aggr = "median")

df_res <- data.frame(
  Metodo          = "MonteCarlo",
  Semana          = seq_along(resultados$lucros),
  Lucro           = resultados$lucros,
  LucroAgregado   = resultados$lucro_agr
)

# --- 8) Saída organizada e exportação ---

# Imprime a tabela completa
cat("\n\n--- Tabela de Lucros por Semana ---\n")
print(df_res)

# Mostra claramente o lucro agregado
cat(sprintf("\nLucro agregado (mediana) = %.2f EUR\n\n", resultados$lucro_agr))

# --- 9) Gráfico simples ---
plot(df_res$Semana, df_res$Lucro,
     type  = "b",
     pch   = 16,
     xlab  = "Semana",
     ylab  = "Lucro (EUR)",
     main  = "Lucro por Semana — Monte Carlo (Fase 2)")
abline(h = resultados$lucro_agr, lty = 2)
legend("topright",
       legend = sprintf("Mediana = %.2f", resultados$lucro_agr),
       lty    = 2,
       bty    = "n")
