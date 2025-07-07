# Script: Otimização Multiobjetivo (Objetivo 2) usando NSGA-II

# 1) Instalar e carregar pacotes necessários
if (!require("mco")) install.packages("mco")
library(mco)

# 2) Carregar funções auxiliares e dados de previsão
source("otiutils.R")   # deve definir calc_upper(), repair(), eval() e pred_sales/weekend serão atribuídos

# 3) Definir função de avaliação multiobjetivo (NSGA-II)
eval2 <- function(x) {
  x_int <- round(x)
  s2    <- repair(x_int)
  
  # isto devolve o lucro (usa pred_sales e weekend do .GlobalEnv)
  profit <- eval(s2)
  
  total_resources <- sum(s2[1:7]) + sum(s2[8:28])
  c(-profit, total_resources)
}

# 4) Função principal para a interface (NSGA-II)
optimize_nsga2 <- function(pred_sales, weekend_vec,
                           n.pop = 100L,
                           n.gen = 49L) {
  # assegura que eval2() encontra estas variáveis
  assign("pred_sales", pred_sales, envir = .GlobalEnv)
  assign("weekend",    weekend_vec, envir = .GlobalEnv)
  
  dim_s <- length(calc_upper(pred_sales))
  lower <- rep(0, dim_s)
  upper <- calc_upper(pred_sales)
  
  res <- nsga2(
    fn           = eval2,
    idim         = dim_s,
    odim         = 2,
    lower.bounds = lower,
    upper.bounds = upper,
    popsize      = n.pop,
    generations  = n.gen
  )
  
  # filtra Pareto
  pareto_idx <- res$pareto.optimal
  pareto_vals <- res$value[pareto_idx, , drop = FALSE]
  
  recursos <- pareto_vals[, 2]
  lucros   <- -pareto_vals[, 1]
  o        <- order(recursos)
  
  list(
    recursos = recursos[o],
    lucros   = lucros[o]
  )
}

# 5) Demonstração rápida (para testes)
# set.seed(42)
# resultado <- optimize_nsga2(pred_sales, weekend_vec, n.pop = 100, n.gen = 49)
# plot(resultado$recursos, resultado$lucros, type="b",
#      xlab="Uso de Recursos", ylab="Lucro (EUR)",
#      main="Frente de Pareto - NSGA-II")
