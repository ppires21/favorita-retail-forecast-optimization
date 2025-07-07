# 0) Fontes das funções
source("otiutils.R")    # repair(), eval(), calc_upper()
set.seed(123)
library(pso)

# 1) Carregar previsões já calculadas
df_pred <- read.csv("ITER_Sales_prev.csv")
sales_cols       <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")
Runs             <- length(unique(df_pred$Iteracao))
forecast_horizon <- max(df_pred$Dia)  # deverá ser 7

# 2) Carregar histórico de vendas
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date      <- as.Date(sales$date)
sales_cleaned   <- sales[1:(nrow(sales) - 3), ]
all_dates       <- tail(sales_cleaned$date, 365)

# 3) Parâmetros da janela deslizante
increment <- 7
W         <- 365 - forecast_horizon - (Runs - 1) * increment

# 4) Reconstruir lista de previsões (matrizes 7x6 por iteração)
pred_list <- vector("list", Runs)
for (b in seq_len(Runs)) {
  sub            <- df_pred[df_pred$Iteracao == b, sales_cols]
  pred_list[[b]] <- as.matrix(sub)
}

# 5) Parâmetros PSO
D           <- 70
lower       <- rep(0, D)
pso_params <- list(
  max_iters   = 100,
  swarm_size  = 50,
  inertia     = 0.5,
  cognitive_c = 1.0,
  social_c    = 2
)

# prepare list to guardar todas as curvas de convergência
profit_hist_list <- vector("list", Runs)

# 6) Otimização PSO para uma única semana (com recolha de stats)
optimiza_semana_pso <- function(pred_matrix) {
  assign("pred_sales", pred_matrix, envir = .GlobalEnv)
  upper <- calc_upper(pred_sales)
  
  obj <- function(s) {
    s_round <- round(s)
    s_rep   <- repair(s_round)
    lucro   <- eval(s_rep)
    return(-lucro)
  }
  
  res <- psoptim(
    par     = rep(NA, D),
    fn      = obj,
    lower   = lower,
    upper   = upper,
    control = list(
      maxit       = pso_params$max_iters,
      s           = pso_params$swarm_size,
      w           = pso_params$inertia,
      c.p         = pso_params$cognitive_c,
      c.g         = pso_params$social_c,
      trace       = 1,          # activa stats
      REPORT      = 1,
      trace.stats = TRUE        # recolhe res$stats$error
    )
  )
  
  # converte o histórico de erro em lucro
  profit_history <- -res$stats$error
  
  list(
    lucro          = -res$value,
    sol            = repair(round(res$par)),
    profit_history = profit_history
  )
}

# 7) Loop sobre todas as semanas
run_phase2_pso <- function(pred_list, aggr = c("mean", "median")) {
  aggr   <- match.arg(aggr)
  lucros <- numeric(length(pred_list))
  sols   <- vector("list", length(pred_list))
  
  for (i in seq_along(pred_list)) {
    start_idx   <- W + (i - 1) * increment + 1
    week_dates  <- all_dates[start_idx:(start_idx + forecast_horizon - 1)]
    Sys.setlocale("LC_TIME", "pt_PT.UTF-8")
    weekend_vec <- weekdays(week_dates) %in% c("sábado", "domingo")
    assign("weekend", weekend_vec, envir = .GlobalEnv)
    
    cat(sprintf("Semana %2d/%d — ", i, length(pred_list)))
    out <- optimiza_semana_pso(pred_list[[i]])
    cat(sprintf("Lucro = %.2f EUR\n", out$lucro))
    
    lucros[i]                  <- out$lucro
    sols[[i]]                  <- out$sol
    profit_hist_list[[i]] <<- out$profit_history
  }
  
  lucro_agr <- if (aggr == "mean") mean(lucros) else median(lucros)
  cat("============================================\n")
  cat(sprintf("%s dos lucros = %.2f EUR\n", toupper(aggr), lucro_agr))
  
  list(
    lucros    = lucros,
    lucro_agr = lucro_agr,
    sols      = sols
  )
}

# 8) Executar
resultados_pso <- run_phase2_pso(pred_list, aggr = "median")

df_pso <- data.frame(
  Metodo        = "PSO",
  Semana        = seq_along(resultados_pso$lucros),
  Lucro         = resultados_pso$lucros,
  LucroAgregado = resultados_pso$lucro_agr
)

# 9) Tabela + exportação
cat("\n\n--- Tabela de Lucros por Semana (PSO) ---\n")
print(df_pso)
cat(sprintf("\nLucro agregado (mediana) = %.2f EUR\n\n", resultados_pso$lucro_agr))
write.csv(df_pso, file = "pso_phase2_results.csv", row.names = FALSE)
cat("Resultados guardados em 'pso_phase2_results.csv'\n")

# 10) Gráfico de lucros por semana
plot(df_pso$Semana, df_pso$Lucro,
     type  = "b",
     pch   = 16,
     xlab  = "Semana", ylab = "Lucro (EUR)",
     main  = "Lucro por Semana — PSO (Fase 2)")
abline(h = resultados_pso$lucro_agr, lty = 2)
legend("topright",
       legend = sprintf("Mediana = %.2f", resultados_pso$lucro_agr),
       lty    = 2, bty = "n")

# 11) Gráfico de convergência — todas as semanas
# número de avaliações por iteração
eval_counts <- seq_len(pso_params$max_iters) * pso_params$swarm_size

# convergência ao longo das avaliações
matplot(
  eval_counts,
  do.call(cbind, profit_hist_list),
  type  = "l", lty = 1, lwd = 1.5,
  xlab  = "Avaliações da função",
  ylab  = "Lucro (EUR)",
  main  = "Convergência do PSO — Fase 2",
  col   = rainbow(length(profit_hist_list))
)
legend(
  "bottomright",
  legend = paste("Semana", seq_along(profit_hist_list)),
  col    = rainbow(length(profit_hist_list)),
  lty    = 1, bty = "n", cex = 0.8
)