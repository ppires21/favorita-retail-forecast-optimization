# ------------------------------------------------------------
# Configuração inicial
# ------------------------------------------------------------
set.seed(45)
source("C:/Users/joaoa/OneDrive/Documentos/Rstudio/Otimização/otiutils.R")

if (!requireNamespace("GA", quietly = TRUE)) install.packages("GA")
library(GA)

# ------------------------------------------------------------
# Preparar limites da solução
# ------------------------------------------------------------
dimension <- length(calc_upper(pred_sales))
lower <- rep(0, dimension)
upper <- calc_upper(pred_sales)

# ------------------------------------------------------------
# Função de fitness com penalização pelo uso de recursos
# alpha define o peso da penalização
# ------------------------------------------------------------
create_fitness_weighted <- function(alpha) {
  function(par) {
    s <- repair(round(par))
    lucro <- eval(s)
    recursos <- sum(s[1:7]) + sum(matrix(s[8:28], nrow = 7, ncol = 3, byrow = TRUE))
    return(lucro - alpha * recursos)  # função escalar
  }
}

# ------------------------------------------------------------
# Parâmetros GA
# ------------------------------------------------------------
gen_max    <- 100
pop_size   <- 50
elite_frac <- 0.1
pmutation  <- 0.1
pcrossover <- 0.7
run_stop   <- 20

# ------------------------------------------------------------
# Testar vários alphas e guardar soluções
# ------------------------------------------------------------
alphas <- c(0.01, 0.05, 0.1, 0.2, 0.4)
resultados <- data.frame(Lucro = numeric(), Recursos = numeric(), Alpha = numeric())

for (a in alphas) {
  cat("\n--- Executando GA com alpha =", a, "---\n")
  
  ga_run <- ga(
    type          = "real-valued",
    fitness       = create_fitness_weighted(a),
    lower         = lower,
    upper         = upper,
    popSize       = pop_size,
    maxiter       = gen_max,
    pmutation     = pmutation,
    pcrossover    = pcrossover,
    elitism       = ceiling(pop_size * elite_frac),
    run           = run_stop,
    keepBest      = TRUE,
    parallel      = FALSE,
    seed          = 45
  )
  
  best_par <- ga_run@solution[1, ]
  best_sol <- repair(round(best_par))
  lucro <- eval(best_sol)
  recursos <- sum(best_sol[1:7]) + sum(matrix(best_sol[8:28], nrow = 7, ncol = 3, byrow = TRUE))
  
  resultados <- rbind(resultados, data.frame(
    Lucro = lucro,
    Recursos = recursos,
    Alpha = a
  ))
}

# ------------------------------------------------------------
# Visualização com Pseudo-Frente de Pareto para GA
# ------------------------------------------------------------
plot(resultados$Recursos, resultados$Lucro,
     xlab = "Uso de Recursos",
     ylab = "Lucro (EUR)",
     main = "Pseudo-Frente de Pareto — GA com Penalização",
     pch = 20, col = "lightgray")

# Marcar todos os pontos coloridos
points(resultados$Recursos, resultados$Lucro, pch = 19, col = "blue")

# Identificar e ordenar os pontos de Pareto
objetivos <- as.matrix(resultados[, c("Recursos", "Lucro")])
pareto_idx <- paretoFilter(objetivos, minimize = c(TRUE, FALSE))
pareto_resultados <- resultados[pareto_idx, ]
pareto_resultados <- pareto_resultados[order(pareto_resultados$Recursos), ]

# Destacar os pontos de Pareto
points(pareto_resultados$Recursos, pareto_resultados$Lucro,
       pch = 19, col = "black")

# Ligar os pontos de Pareto com linha preta
lines(pareto_resultados$Recursos, pareto_resultados$Lucro,
      type = "o", col = "black", lwd = 2)

# Rótulos apenas nos pontos da curva
text(pareto_resultados$Recursos, pareto_resultados$Lucro,
     labels = paste0("α=", pareto_resultados$Alpha),
     pos = 4, cex = 0.8, col = "darkred")

