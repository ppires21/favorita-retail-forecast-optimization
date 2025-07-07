# Script: Otimização Multiobjetivo (Objetivo 2) usando NSGA-II

# 1) Instalar e carregar pacotes necessários
if (!require("mco")) install.packages("mco")
library(mco)

# 2) Carregar funções auxiliares e dados de previsão
source("otiutils.R")

# 3) Definir função de avaliação multiobjetivo
#    - Objetivo 1: maximizar lucro (usa-se -lucro para minimização)
#    - Objetivo 2: minimizar soma de recursos (armazém + veículos)
eval2 <- function(x) {
  # Forçar variáveis de decisão a inteiros
  x_int <- round(x)
  s2    <- repair(x_int)
  
  # Avaliar lucro (função externa)
  profit <- {
    capture.output(tmp <- eval(s2))
    tmp
  }
  
  # Calcular uso total de recursos: soma de W (7) + soma de V (7×3)
  # já são inteiros, então soma direta
  total_resources <- sum(s2[1:7]) + sum(s2[8:28])
  
  # Retorna vetor de objetivos: (-lucro, total_recursos)
  return(c(-profit, total_resources))
}

# 4) Definir dimensão e limites do espaço de busca
dim_s <- length(calc_upper(pred_sales))    # deve ser 70
lower <- rep(0, dim_s)
upper <- calc_upper(pred_sales)

# 5) Configurar parâmetros do NSGA-II
pop_size <- 100     # tamanho da população
num_gens <- 49     # número de gerações
set.seed(42)        # para reprodutibilidade

# 6) Executar NSGA-II
resultado <- nsga2(
  fn           = eval2,
  idim         = dim_s,
  odim         = 2,
  lower.bounds = lower,
  upper.bounds = upper,
  popsize      = pop_size,
  generations  = num_gens
)

# 7) Extrair soluções Pareto ótimas da última geração
pareto_mask  <- resultado$pareto.optimal
pareto_pars  <- resultado$par[pareto_mask, ]
pareto_vals  <- resultado$value[pareto_mask, ]

# 8) Visualizar Frente de Pareto
#    Converte objetivo 1 de minimização (-lucro) para lucro positivo
profit_vals   <- -pareto_vals[, 1]
resource_vals <-  pareto_vals[, 2]
# Ordenar pela utilização de recursos para ligar pontos consecutivos corretamente
ord <- order(resource_vals)
res_ord <- resource_vals[ord]
prof_ord <- profit_vals[ord]

# Plot inicial e linhas ligando apenas pontos adjacentes
plot(
  res_ord, prof_ord,
  xlab = "Uso de Recursos",
  ylab = "Lucro (EUR)",
  main = "Curva de Pareto - NSGA-II",
  type = "b",    # pontos conectados por linha entre vizinhos
  pch  = 16
)
