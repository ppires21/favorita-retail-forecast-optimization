# -------------------------------
# Carregar utilitários e métodos
source("otiutils.R")
source("hill.R")
set.seed(45)

# -------------------------------
# Variáveis globais para análise de convergência
MAXIT <- 5000         # número de iterações
EV <- 0                # contador de avaliações
BEST <- -Inf           # melhor lucro observado até agora
F <- rep(NA, MAXIT)    # histórico do melhor lucro acumulado

# -------------------------------
# Função silenciosa + monitorização de convergência
eval_silent <- function(s, pred_sales) {
  sink(tempfile())
  on.exit(sink())
  
  res <- eval(s)
  EV <<- EV + 1
  BEST <<- max(BEST, res)
  if (EV <= MAXIT) F[EV] <<- BEST
  
  return(res)
}

# -------------------------------
# Função de mutação customizada: apenas altera D (posições 29:70)
rchange_custom <- function(par, lower, upper) {
  par_new <- par
  delta <- rnorm(42, mean = 0, sd = 10)
  par_new[29:70] <- par_new[29:70] + delta
  return(par_new)
}

# -------------------------------
# Parâmetros de otimização
N <- MAXIT
lower <- rep(0, 70)
upper <- calc_upper(pred_sales)

# Solução inicial baseada nas previsões
initial_par <- rep(0, 70)
initial_par[29:70] <- as.vector(t(pred_sales))
initial_par <- repair(initial_par)

# -------------------------------
# Executar Hill Climbing
best <- hclimbing(
  par = initial_par,
  fn = function(s) eval_silent(repair(s), pred_sales),
  change = rchange_custom,
  lower = lower,
  upper = upper,
  control = list(maxit = N, REPORT = 1000, digits = 0),
  type = "max"
)

# -------------------------------
# Resultados
cat(">> Melhor lucro encontrado:", best$eval, "EUR\n")
cat(">> Melhor solução encontrada:\n")
print(round(best$sol))

cat("\n=======================\n")
cat(">> Avaliação da melhor solução encontrada:\n")
eval(repair(best$sol))

# -------------------------------
# Gráfico de convergência
plot(F, type = "l", col = "darkgreen", lwd = 2,
     main = "Convergência — Hill Climbing",
     xlab = "Avaliações", ylab = "Melhor Lucro Acumulado")
