# pso_tuning_phase2.R

# 0) Pacotes e fontes
if (!require(pso)) install.packages("pso", dependencies = TRUE)
library(pso)
library(rminer)
library(forecast)

source("otiutils.R")   # repair(), eval(), calc_upper()
set.seed(123)          # semente para reprodutibilidade

# 1) Dados e janelas
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date      <- as.Date(sales$date)
sales_cleaned   <- sales[1:(nrow(sales) - 3), ]
all_dates       <- tail(sales_cleaned$date, 365)

# Carrega previsões geradas (Iterações × 7 dias × 6 séries)
df_pred         <- read.csv("ITER_Sales_prev.csv")
sales_cols      <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")
Runs            <- length(unique(df_pred$Iteracao))
forecast_horizon<- max(df_pred$Dia)   # 7
increment       <- 7
W               <- 365 - forecast_horizon - (Runs - 1) * increment

# Reconstrói lista de matrizes 7×6
pred_list <- lapply(1:Runs, function(b) {
  as.matrix(df_pred[df_pred$Iteracao==b, sales_cols])
})

# 2) Grid de tuning
param_grid <- expand.grid(
  swarm_size = c(50),
  inertia    = c(0.5, 0.7, 0.9),
  c1         = c(1.0, 1.5, 2.0),
  c2         = c(1.0, 1.5, 2.0),
  max_iters  = c(100)
)

results <- param_grid
results$profit  <- NA_real_
results$runtime <- NA_real_

# 3) Loop de tuning
for (i in seq_len(nrow(param_grid))) {
  params <- param_grid[i, ]
  cat(sprintf(
    "\nTestando (%d/%d): s=%d, w=%.2f, c1=%.1f, c2=%.1f, iters=%d\n",
    i, nrow(param_grid),
    params$swarm_size, params$inertia,
    params$c1, params$c2, params$max_iters
  ))
  set.seed(123)
  start_time <- Sys.time()
  
  lucros <- numeric(Runs)
  # para cada semana
  for (b in seq_len(Runs)) {
    # 3.1) define pred_sales e weekend
    mat <- pred_list[[b]]
    assign("pred_sales", mat, envir = .GlobalEnv)
    start_idx  <- W + (b-1)*increment + 1
    week_dates <- all_dates[start_idx:(start_idx + forecast_horizon - 1)]
    Sys.setlocale("LC_TIME", "pt_PT.UTF-8")
    weekend_vec <- weekdays(week_dates) %in% c("sábado","domingo")
    assign("weekend", weekend_vec, envir = .GlobalEnv)
    
    # 3.2) chama PSO
    upper <- calc_upper(pred_sales)
    res <- psoptim(
      par   = rep(NA, length(upper)),
      fn    = function(s) {
        s2 <- repair(round(s))
        -eval(s2)
      },
      lower = rep(0, length(upper)),
      upper = upper,
      control = list(
        maxit = params$max_iters,
        s     = params$swarm_size,
        w     = params$inertia,
        c.p   = params$c1,
        c.g   = params$c2,
        trace = 0
      )
    )
    lucros[b] <- -res$value
  }
  
  end_time <- Sys.time()
  results$profit[i]  <- median(lucros)  # ou mean(lucros)
  results$runtime[i] <- as.numeric(difftime(end_time, start_time, units="secs"))
  cat(sprintf(
    "-> Mediana dos lucros = %.2f EUR, Tempo total = %.2f seg\n",
    results$profit[i], results$runtime[i]
  ))
}

# 4) Melhor combinação
best_idx    <- which.max(results$profit)
best_params <- results[best_idx, ]
cat("\nMelhor configuração encontrada:\n")
print(best_params)
