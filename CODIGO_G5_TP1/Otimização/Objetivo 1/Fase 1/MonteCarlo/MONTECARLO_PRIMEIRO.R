source("otiutils.R")
source("montecarlo.R")
source("blind.R")
set.seed(12345)

# PARA A EVAL NÃO DAR OUTPUT ENQUANTO CORRE
eval_silent <- function(s, pred_sales) {
  sink(tempfile())
  on.exit(sink())
  return(eval(s))
}

# Parâmetros
N <- 10000
lower <- rep(0, 70)
upper <- calc_upper(pred_sales)

# Otimização com mcsearch
best <- mcsearch(
  fn = function(s) eval_silent(repair(s), pred_sales),
  lower = lower,
  upper = upper,
  N = N,
  type = "max"
)

cat(">> Melhor lucro encontrado:", best$eval, "EUR\n")
cat(">> Melhor solução encontrada:\n")
print(round(best$sol))

cat("\n=======================\n")
cat(">> Avaliação da melhor solução encontrada:\n")
eval(repair(best$sol))