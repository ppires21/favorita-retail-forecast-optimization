# DE_MAIN_CONVERGENCE.R

# — Instalar e carregar pacote DEoptim (se necessário) —
if (!require(DEoptim)) {
  install.packages("DEoptim", dependencies = TRUE)
}
library(DEoptim)

# — Carregar funções utilitárias e dados —
source("otiutils.R")   # contém: pred_sales, eval, repair, calc_upper

# — Reprodutibilidade —
set.seed(123)

# — Definições de dimensão e limites —
D     <- 70
lower <- rep(0, D)
upper <- calc_upper(pred_sales)

# — Função-objetivo (maximizar lucro) —
#    devolve negativo para que o DEoptim minimize
obj_de <- function(s) {
  s      <- round(s)       # inteiros
  s      <- repair(s)      # viabilidade
  profit <- eval(s)        # lucro
  return(-profit)
}

# — Parâmetros gerais (herdados do PSO) e de DE —
params <- list(
  max_iters = 100,   # iterações
  pop_size  = 50,     # tamanho da “swarm” / população
  F         = 0.8,    # fator de mutação
  CR        = 0.9     # prob. de crossover
)

cat(">> A correr DE com os seguintes parâmetros:\n")
print(params)

# — Configuração do controle para DEoptim —
ctrl <- DEoptim.control(
  NP           = params$pop_size,
  itermax      = params$max_iters,
  F            = params$F,
  CR           = params$CR,
  trace        = 100,         # imprime progresso a cada 100 gerações
  storepopfrom = 1            # guarda histórico desde a 1ª iteração
)


# — Executa o Differential Evolution e mede tempo —
start_time <- Sys.time()

res <- DEoptim(
  fn      = obj_de,
  lower   = lower,
  upper   = upper,
  control = ctrl
)

end_time <- Sys.time()
runtime  <- as.numeric(difftime(end_time, start_time, units = "secs"))

# — Extrair e processar solução ótima —
best_raw    <- res$optim$bestmem
best_sol    <- repair(best_raw)
best_profit <- -res$optim$bestval

# — Separar vetores de decisão —
W_opt <- best_sol[1:7]
V_opt <- matrix(best_sol[8:28], nrow = 7, byrow = TRUE)
colnames(V_opt) <- c("v1", "v2", "v3")
D_opt <- matrix(best_sol[29:70], nrow = 7, byrow = TRUE)
colnames(D_opt) <- c("wd1", "wd2", "we1", "we2", "wb1", "wb2")

# — Exibir resultados finais —
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

# — Análise de convergência —
# res$member$bestvalit contém o mínimo do objetivo (negativo do lucro) em cada iteração
error_history  <- res$member$bestvalit
profit_history <- -error_history

plot(profit_history, type = "l",
     xlab = "Geração", ylab = "Lucro otimizado",
     main = "Convergência do Differential Evolution")
