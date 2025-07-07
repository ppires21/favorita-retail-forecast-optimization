# pso_main.R

# Carrega pacotes necessários
if (!require(pso)) {
  install.packages("pso", dependencies = TRUE)
}
library(pso)

# Carrega utilitários (Objetivo 1)
source("otiutils.R")

# Definição do problema - Objetivo 1
D <- 70                            # dimensão do vetor de solução
lower <- rep(0, D)                 # limites inferiores
upper <- calc_upper(pred_sales)  # limites superiores para o Objetivo 1

# Função objetivo para PSO (maximização do lucro)
# Como o psoptim minimiza por defeito, invertemos o sinal do lucro
obj_pso <- function(s) {
  s <- round(s)       # garante inteiros
  s <- repair(s)      # repara soluções inviáveis
  profit <- eval(s)   # calcula lucro da semana
  return(-profit)     # devolve valor a minimizar (lucro negativo)
}

# Grid de parâmetros para tuning
param_grid <- expand.grid(
  swarm_size = c(50),
  inertia    = c(0.5, 0.9),
  c1         = c(1.0, 2.0),
  c2         = c(1.0, 2.0),
  max_iters  = c(100)
)

# Data frame para resultados
results <- param_grid
results$profit  <- NA_real_
results$runtime <- NA_real_

# Loop de tuning
for (i in seq_len(nrow(param_grid))) {
  params <- param_grid[i, ]
  cat(sprintf("\nTestando parâmetros (linha %d/%d): swarm_size=%d, inertia=%.2f, c1=%.2f, c2=%.2f, max_iters=%d\n", 
              i, nrow(param_grid), 
              params$swarm_size, params$inertia, params$c1, params$c2, params$max_iters))
  set.seed(123)
  # Medir tempo de execução
  start_time <- Sys.time()
  res <- psoptim(
    par     = rep(NA, D),
    fn      = obj_pso,
    lower   = lower,
    upper   = upper,
    control = list(
      maxit = params$max_iters,
      s     = params$swarm_size,
      w     = params$inertia,
      c.p   = params$c1,
      c.g   = params$c2,
      trace = 0
    )
  )
  end_time <- Sys.time()
  
  # Extrair lucro e tempo
  profit <- -res$value
  runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  results$profit[i]  <- profit
  results$runtime[i] <- runtime
  cat(sprintf("-> Lucro: %.2f EUR, Tempo: %.2f seg\n", profit, runtime))
}

# Selecionar melhor combinação
best_idx <- which.max(results$profit)
best_params <- results[best_idx, ]
cat(sprintf("\nMelhor configuração encontrada:\n"))
print(best_params)

# Guardar resultados num ficheiro CSV (opcional)
# write.csv(results, "pso_tuning_results.csv", row.names = FALSE)

# Fim do tuning
