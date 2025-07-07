# Carregar utilitários e métodos
source("C:/Users/joaoa/OneDrive/Documentos/Rstudio/Otimização/otiutils.R")
set.seed(45)

# -------------------------------
# Renomear a função de avaliação para evitar shadowing
eval_profit <- eval
rm(eval)

# Função silenciosa para avaliação (sem prints excessivos)
eval_silent <- function(par) {
  sink(tempfile())
  on.exit(sink())
  res <- eval_profit(repair(par))
  # Assegura retorno numérico válido
  res <- as.numeric(res)
  if (is.na(res) || !is.finite(res)) res <- -Inf
  return(res)
}

# -------------------------------
# Parâmetros de otimização
dimension <- length(calc_upper(pred_sales))
lower <- rep(0, dimension)
upper <- calc_upper(pred_sales)

# Parâmetros GA (carga reduzida)
gen_max    <- 100    # gerações máximas
pop_size   <- 50     # tamanho da população
elite_frac <- 0.1     # elitismo
pmutation  <- 0.1     # taxa de mutação
pcrossover <- 0.7     # taxa de crossover
run_stop   <- 20      # gerações sem melhoria para stop

# -------------------------------
# Função de fitness para GA (maximizar lucro)
fitness_ga <- function(pop) {
  if (is.null(dim(pop))) {
    # Vetor único
    return(eval_silent(pop))
  } else {
    # Vários indivíduos
    return(sapply(seq_len(nrow(pop)), function(i) eval_silent(pop[i, ])))
  }
}


# -------------------------------
# Paralelização opcional
autoregister_parallel <- function() {
  if (requireNamespace("doParallel", quietly = TRUE)) {
    library(doParallel)
    cl <- makeCluster(parallel::detectCores() - 1)
    registerDoParallel(cl)
    return(cl)
  }
  return(NULL)
}
# cl <- autoregister_parallel()
# Vetor para guardar o melhor fitness por geração
convergence_history <- numeric(gen_max)


# -------------------------------
# Executar Algoritmo Genético usando GA, com paralel=FALSE ou TRUE
if (!requireNamespace("GA", quietly = TRUE)) install.packages("GA")
library(GA)

ga_res <- ga(
  type          = "real-valued",
  fitness       = fitness_ga,
  lower         = lower,
  upper         = upper,
  popSize       = pop_size,
  maxiter       = gen_max,
  pmutation     = pmutation,
  pcrossover    = pcrossover,
  elitism       = ceiling(pop_size * elite_frac),
  run           = run_stop,
  parallel      = FALSE,    # usar TRUE se paralel registrou
  keepBest      = TRUE,
  monitor = function(obj) {
    best <- obj@fitness[which.max(obj@fitness)]
    convergence_history[obj@iter] <<- best
    cat(sprintf("Geração %d | Melhor fitness: %.2f\n", obj@iter, best))
  },
  seed          = 45
)

# if (!is.null(cl)) stopCluster(cl)
# Plotar a convergência
plot(
  convergence_history, type = "l", lwd = 2, col = "blue",
  xlab = "Geração", ylab = "Lucro obtido",
  main = "Convergência do Algoritmo Genético"
)


# -------------------------------
# Resultados
best_par <- ga_res@solution[1, ]
best_val <- ga_res@fitnessValue
cat(">> Melhor lucro encontrado (GA):", round(best_val,2), "EUR\n")
cat(">> Melhor solução encontrada (parâmetros):\n")
print(round(best_par))

cat("\n=======================\n")
cat(">> Avaliação detalhada da melhor solução (após repair):\n")
eval_profit(repair(best_par))