# Instala o pacote se necessário
# install.packages("GenSA")
library(GenSA)

# Número de variáveis
n_vars <- 70

# Limites inferiores e superiores
lower <- rep(0, n_vars)
upper <- calc_upper(pred_sales)

# 1) Inicializar vetor global para guardar o histórico dos lucros (lucro avaliado em cada passo)
profit_history <- c()

# 2) Função objetivo (a ser minimizada, então usamos -lucro)
objective_function <- function(s) {
  s <- pmin(pmax(round(s), lower), upper)  # garantir dentro dos limites
  s <- repair(s)                           # tornar solução viável
  profit <- eval(s)                        # calcular lucro
  
  # Guardar lucro no histórico para análise posterior
  if (is.finite(profit)) {
    profit_history <<- c(profit_history, profit)
  }
  
  if (profit < 0) return(1e6)              # penalizar soluções negativas
  return(-profit)                          # minimizar o negativo do lucro
}

# 3) Rodar Simulated Annealing
set.seed(123)
sa_result <- GenSA(
  par = rep(1, n_vars),           # solução inicial viável
  lower = lower,
  upper = upper,
  fn = objective_function,
  control = list(max.call = 5000, verbose = TRUE)
)

# 4) Melhor solução encontrada
best_solution <- round(sa_result$par)
best_solution <- pmin(pmax(best_solution, lower), upper)
best_solution_repaired <- repair(best_solution)

cat(">> Melhor lucro encontrado (SA):", eval(best_solution_repaired), "EUR\n")
cat(">> Solução ótima (vetor s):\n")
print(best_solution_repaired)
cat("\n\n\n")
print(eval(best_solution_repaired))

# 5) Gráfico da convergência do Simulated Annealing
plot(profit_history, type = "l", lwd = 2, col = "blue",
     xlab = "Avaliações da função objetivo",
     ylab = "Lucro avaliado",
     main = "Convergência do Simulated Annealing")
grid()
