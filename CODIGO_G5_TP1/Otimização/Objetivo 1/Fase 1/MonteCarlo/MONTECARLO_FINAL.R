# MONTECARLO_COM_CONVERGENCIA.R

source("otiutils.R")
source("montecarlo.R")
source("blind.R")
set.seed(12345)

# ——— Função silenciosa para evitar output durante as avaliações ———
eval_silent <- function(s, pred_sales) {
  sink(tempfile())
  on.exit(sink())
  eval(s)  # usa o pred_sales que está no GlobalEnv
}

# ——— Parâmetros ———
N     <- 5000
lower <- rep(0, 70)
upper <- calc_upper(pred_sales)

# ——— Configurar variáveis globais para monitorização ———
# função auxiliar idêntica ao demo
g_best <- function(val1, val2, type="max") {
  if (type=="min") min(c(val1,val2)) else max(c(val1,val2))
}

TYPE  <- "max"         # como o mcsearch está a maximizar
EV    <- 0             # contador de avaliações
BEST  <- -Inf          # melhor valor até agora
F     <- rep(NA, N)    # vetor para guardar a convergência

# ——— Função de avaliação monitorizada ———
m_eval <- function(s) {
  # usa a tua repair e eval_silent
  sol  <- repair(round(s))
  res  <- eval_silent(sol, pred_sales)
  # atualiza estatísticas
  EV   <<- EV + 1
  BEST <<- g_best(BEST, res, TYPE)
  if (EV <= N) F[EV] <<- BEST
  return(res)
}

# ——— Otimização com mcsearch usando a função monitorizada ———
best <- mcsearch(
  fn    = m_eval,
  lower = lower,
  upper = upper,
  N     = N,
  type  = "max"
)

# ——— Resultados finais ———
cat(">> Melhor lucro encontrado:", best$eval, "EUR\n")
cat(">> Melhor solução encontrada:\n")
print(round(best$sol))

cat("\n=======================\n")
cat(">> Avaliação da melhor solução encontrada:\n")
eval(repair(best$sol))

# ——— Gráfico de convergência ———
plot(
  F,
  type  = "l",
  lwd   = 2,
  col   = "blue",
  main  = "Convergência do Monte Carlo",
  xlab  = "Número de Avaliações",
  ylab  = "Melhor Lucro Até Agora (EUR)"
)