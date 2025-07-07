# Instalar e carregar pacote pso (se necessário)
if (!require(pso)) {
  install.packages("pso", dependencies = TRUE)
}
library(pso)

# Carregar ficheiros com funções utilitárias e dados
source("otiutils.R")   # contém: pred_sales, eval, repair, calc_upper
set.seed(123)

# Definições de dimensão e limites
D <- 70
lower <- rep(0, D)
upper <- calc_upper(pred_sales)

# Função objetivo (para maximizar lucro)
obj_pso <- function(s) {
  s <- round(s)       # arredonda para inteiros
  s <- repair(s)      # ajusta para solução viável
  profit <- eval(s)   # calcula lucro
  return(-profit)     # negativo porque psoptim minimiza
}

# Parâmetros PSO (única combinação), com 10 000 iterações
params <- list(
  max_iters    = 100,
  swarm_size   = 50,
  inertia      = 0.5,
  cognitive_c  = 1.0,
  social_c     = 2
)

# Exibição dos parâmetros usados
cat(">> A correr PSO com os seguintes parâmetros:\n")
print(params)

# Lançar PSO e medir tempo de execução
set.seed(123)
start_time <- Sys.time()

res <- psoptim(
  par   = rep(NA, D),
  fn    = obj_pso,
  lower = lower,
  upper = upper,
  control = list(
    maxit        = params$max_iters,
    s            = params$swarm_size,
    w            = params$inertia,
    c.p          = params$cognitive_c,
    c.g          = params$social_c,
    trace        = 1,
    REPORT       = 1,
    trace.stats  = TRUE      # recolhe estatísticas de cada iteração
  )
)

end_time <- Sys.time()
runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

# Processar solução final
best_raw    <- res$par
best_sol    <- repair(best_raw)
best_profit <- -res$value

# Separar vetores de decisão
W_opt <- best_sol[1:7]
V_opt <- matrix(best_sol[8:28], nrow = 7, byrow = TRUE)
colnames(V_opt) <- c("v1", "v2", "v3")
D_opt <- matrix(best_sol[29:70], nrow = 7, byrow = TRUE)
colnames(D_opt) <- c("wd1", "wd2", "we1", "we2", "wb1", "wb2")

# Mostrar resultados finais
cat(sprintf("\n>> Lucro total obtido: %.2f EUR\n", best_profit))
cat(sprintf(">> Tempo de execução: %.2f segundos\n\n", runtime))
cat("Recursos de armazém ótimos (W):\n")
print(W_opt)
cat("\nRecursos de veículos ótimos (V):\n")
print(V_opt)
cat("\nDistribuição ótima de produtos (D):\n")
print(D_opt)
cat("\n=======================\n")
cat(">> Avaliação da melhor solução encontrada:\n")
eval(repair(best_sol))

# Análise de convergência
# Extrair histórico de erro (função objetivo minimizada) e converter para lucro
error_history  <- res$stats$error
profit_history <- -error_history

# Gráfico de convergência (lucro vs. iteração)
# repete cada valor de lucro 50 vezes
profit_eval <- rep(profit_history, each = params$swarm_size)

plot(seq_along(profit_eval), profit_eval, type = "l",
     xlab = "Avaliações da função",
     ylab = "Lucro otimizado",
     main = "Convergência do PSO — Fase 1")
