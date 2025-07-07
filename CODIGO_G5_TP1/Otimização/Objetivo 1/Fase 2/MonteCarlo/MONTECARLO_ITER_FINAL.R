# ficheiro: MONTECARLO_ITER_GROWING_COM_CONVERGENCIA.R

# 0) Fontes das funções de otimização e semente
source("otiutils.R")    # repair(), eval(), calc_upper()
source("montecarlo.R")  # mcsearch()
source("blind.R")       # fsearch(), dfsearch()
set.seed(12345)

# 1) Carregar previsões
df_pred <- read.csv("ITER_Sales_prev.csv")
sales_cols       <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")
Runs             <- length(unique(df_pred$Iteracao))
forecast_horizon <- max(df_pred$Dia)

# 2) Carregar histórico de vendas
sales <- read.csv("sales_clean.csv", sep=";")
sales$date      <- as.Date(sales$date)
sales_cleaned   <- sales[1:(nrow(sales)-3), ]
all_dates       <- tail(sales_cleaned$date, 365)

# 3) Parâmetros do growing window
increment <- 7
W         <- 365 - forecast_horizon - (Runs - 1) * increment

# 4) Reconstruir lista de previsões
pred_list <- vector("list", Runs)
for (b in seq_len(Runs)) {
  sub            <- df_pred[df_pred$Iteracao == b, sales_cols]
  pred_list[[b]] <- as.matrix(sub)
}

# 5) Função de otimização com convergência
optimiza_semana_convergencia <- function(pred_matrix, N = 5000) {
  assign("pred_sales", pred_matrix, envir = .GlobalEnv)
  lower <- rep(0, 70)
  upper <- calc_upper(pred_sales)
  
  F     <- rep(NA, N)
  BEST  <- -Inf
  EV    <- 0
  
  eval_silent <- function(s, pred_sales) {
    sink(tempfile())
    on.exit(sink())
    eval(s)
  }
  
  g_best <- function(val1, val2) max(c(val1, val2))
  
  m_eval <- function(s) {
    sol  <- repair(round(s))
    res  <- eval_silent(sol, pred_sales)
    EV   <<- EV + 1
    BEST <<- g_best(BEST, res)
    if (EV <= N) F[EV] <<- BEST
    return(res)
  }
  
  res <- mcsearch(
    fn    = m_eval,
    lower = lower,
    upper = upper,
    N     = N,
    type  = "max"
  )
  
  list(
    lucro = res$eval,
    sol   = round(res$sol),
    F     = F
  )
}

# 6) Loop por iteração
run_phase2_convergencia <- function(pred_list, N = 5000, aggr = c("mean", "median")) {
  aggr   <- match.arg(aggr)
  lucros <- numeric(length(pred_list))
  sols   <- vector("list", length(pred_list))
  F_list <- vector("list", length(pred_list))
  
  for (i in seq_along(pred_list)) {
    start_idx  <- W + (i-1) * increment + 1
    week_dates <- all_dates[start_idx:(start_idx + forecast_horizon - 1)]
    Sys.setlocale("LC_TIME", "pt_PT.UTF-8")
    weekend_vec <- weekdays(week_dates) %in% c("sábado", "domingo")
    assign("weekend", weekend_vec, envir = .GlobalEnv)
    
    cat(sprintf("Semana %2d/%d — ", i, length(pred_list)))
    out <- optimiza_semana_convergencia(pred_list[[i]], N = N)
    cat(sprintf("Lucro = %.2f EUR\n", out$lucro))
    lucros[i] <- out$lucro
    sols[[i]] <- out$sol
    F_list[[i]] <- out$F
  }
  
  lucro_agr <- if (aggr == "mean") mean(lucros) else median(lucros)
  cat("============================================\n")
  cat(sprintf("%s dos lucros = %.2f EUR\n", toupper(aggr), lucro_agr))
  
  list(
    lucros    = lucros,
    lucro_agr = lucro_agr,
    sols      = sols,
    F_list    = F_list
  )
}

# 7) Executar otimização com convergência
resultados <- run_phase2_convergencia(pred_list, N = 5000, aggr = "median")

df_res <- data.frame(
  Metodo        = "MonteCarlo",
  Semana        = seq_along(resultados$lucros),
  Lucro         = resultados$lucros,
  LucroAgregado = resultados$lucro_agr
)

# 8) Imprimir resultados e gráfico
cat("\n\n--- Tabela de Lucros por Semana ---\n")
print(df_res)
cat(sprintf("\nLucro agregado (mediana) = %.2f EUR\n\n", resultados$lucro_agr))

# Gráfico por semana
plot(df_res$Semana, df_res$Lucro,
     type  = "b",
     pch   = 16,
     xlab  = "Semana", ylab = "Lucro (EUR)",
     main  = "Lucro por Semana — Monte Carlo (Fase 2)")
abline(h = resultados$lucro_agr, lty = 2)
legend("topright",
       legend = sprintf("Mediana = %.2f", resultados$lucro_agr),
       lty    = 2, bty = "n")

# 9) Gráfico de convergência com múltiplas cores
matplot(
  do.call(cbind, resultados$F_list),
  type = "l", lty = 1, lwd = 1.5,
  xlab = "Número de Avaliações",
  ylab = "Melhor Lucro Até Agora (EUR)",
  main = "Convergência por Semana — Monte Carlo",
  col  = rainbow(length(resultados$F_list))
)
legend("bottomright", legend = paste("Semana", seq_along(resultados$F_list)),
       col = rainbow(length(resultados$F_list)),
       lty = 1, bty = "n", cex = 0.8)