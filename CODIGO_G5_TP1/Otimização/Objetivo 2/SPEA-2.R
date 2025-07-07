# Script: SPEA2_manual.R

# 0) Instalar dependências (só 1ª vez)
if (!requireNamespace("emoa", quietly = TRUE)) {
  install.packages("emoa")
}
library(emoa)      
library(ecr)

col_sort_matrix <- function(M, column = 1) {
  if (is.matrix(M)) {
    I <- order(M[, column])
    M <- M[I, , drop = FALSE]
  }
  M
}

source("otiutils.R")  # pred_sales, repair(), eval(), calc_upper()
set.seed(42)


# 1) Definir os limites e a função multi-objetivo
dim_s <- length(calc_upper(pred_sales))  # deverá dar 70
lower <- rep(0, dim_s)
upper <- calc_upper(pred_sales)

obj_fun <- function(x) {
  # x: vetor numérico de comprimento dim_s
  s2 <- repair(round(x))
  lucro <- eval(s2)  # invoca a tua função de lucro
  # total de recursos = soma de W (7) + soma de V (7×3)
  total_recursos <- sum(s2[1:7]) +
    sum(matrix(s2[8:28], nrow = 7, ncol = 3, byrow = TRUE))
  # SPEA-2 minimiza objetivos, logo devolvemos:
  #   (−lucro, total_recursos)
  c(-lucro, total_recursos)
}

# 2) Implementação “manual” do SPEA-2
spea2_manual <- function(fn, lower, upper,
                         n.pop    = 100L,
                         n.arch   = 100L,
                         n.gen    = 49L) {
  n.var <- length(lower)
  # 2.1 População inicial
  P <- replicate(n.pop,
                 runif(n.var, min = lower, max = upper),
                 simplify = FALSE)
  V_P <- t(sapply(P, fn))
  A    <- P
  V_A  <- V_P
  
  for (gen in seq_len(n.gen)) {
    # 2.2 União população + arquivo
    U   <- c(P, A)
    V_U <- rbind(V_P, V_A)
    M   <- nrow(V_U)
    
    # 2.3 Força de cada indivíduo i (nº que domina)
    S <- integer(M)
    for (i in seq_len(M)) {
      S[i] <- sum(apply(V_U, 1, function(vj) dominates(V_U[i,], vj)))
    }
    
    # 2.4 Fitness bruto R[i] = soma das forças de quem domina i
    R <- integer(M)
    for (i in seq_len(M)) {
      R[i] <- sum(S[apply(V_U, 1, function(vj) dominates(vj, V_U[i,]))])
    }
    
    # 2.5 Estimação de densidade (k-ésimo vizinho)
    k <- floor(sqrt(M))
    # Normaliza objetivos para [0,1]
    mins <- apply(V_U, 2, min); maxs <- apply(V_U, 2, max)
    Vn   <- sweep(sweep(V_U, 2, mins, "-"), 2, maxs - mins, "/")
    Dmat <- as.matrix(dist(Vn))
    Dens <- numeric(M)
    for (i in seq_len(M)) {
      dists <- sort(Dmat[i, -i])
      Dens[i] <- 1 / (dists[k] + 2)
    }
    
    # 2.6 Fitness final
    Fit <- R + Dens
    
    # 2.7 Seleção ambiental → novo arquivo A
    idx   <- order(Fit)
    pick  <- idx[seq_len(min(n.arch, length(idx)))]
    A     <- U[pick]
    V_A   <- V_U[pick, , drop = FALSE]
    
    # 2.8 Torneio binário no arquivo para selecionar pais
    Fit_A <- Fit[pick]
    Mpool <- vector("list", n.pop)
    for (j in seq_len(n.pop)) {
      cands <- sample(length(A), 2)
      winner <- if (Fit_A[cands[1]] < Fit_A[cands[2]]) cands[1] else cands[2]
      Mpool[[j]] <- A[[winner]]
    }
    
    # 2.9 Variação (crossover uniforme + mutação Gaussiana)
    Off <- vector("list", n.pop)
    for (j in seq(1, n.pop, by = 2)) {
      p1 <- Mpool[[j]]; p2 <- Mpool[[min(j+1, n.pop)]]
      alpha <- runif(n.var)
      c1 <- alpha * p1 + (1 - alpha) * p2
      c2 <- alpha * p2 + (1 - alpha) * p1
      # mutação (σ = 10% do intervalo)
      c1 <- pmin(upper, pmax(lower, c1 + rnorm(n.var, 0, 0.1 * (upper - lower))))
      c2 <- pmin(upper, pmax(lower, c2 + rnorm(n.var, 0, 0.1 * (upper - lower))))
      Off[[j]] <- c1
      if (j + 1 <= n.pop) Off[[j+1]] <- c2
    }
    
    # 2.10 Próxima geração
    P   <- Off
    V_P <- t(sapply(P, fn))
  }
  
  list(archive    = A,
       objectives = V_A)
}

# 3) Correr o SPEA-2
res <- spea2_manual(obj_fun, lower, upper,
                    n.pop  = 100L,
                    n.arch = 100L,
                    n.gen  = 49L)

# 4) Extrair Pareto-front bruto da última geração
pf <- res$objectives           # matriz n×2: col1=–lucro, col2=recursos

# 5) Filtrar com mco::paretoFilter — tal como faz o prof no exemplo nsga2:
if (!requireNamespace("mco", quietly = TRUE)) install.packages("mco")
library(mco)
pf_nd <- mco::paretoFilter(pf) # devolve só os pontos não-dominados

# 6) Ordenar pelo uso de recursos (coluna 2) “à maneira do prof”:
P_nd_sorted <- col_sort_matrix(pf_nd, 2)

# 7) Extrair os vectores para plotar (e inverter o lucro)
recursos <-  P_nd_sorted[, 2]
lucros   <- -P_nd_sorted[, 1]  # volta a pôr o lucro em positivo

# 8) Plot “ligado” exatamente como faz o professor
plot(recursos, lucros,
     type  = "b",    # both: linhas + pontos
     pch   = 16,      
     xlab  = "Uso de Recursos",
     ylab  = "Lucro (EUR)",
     main  = "Frente de Pareto — SPEA-2 Manual")

# 9) Tabela com as primeiras soluções
solucoes <- data.frame(
  Lucro    = round(lucros,   2),
  Recursos = recursos
)
print(head(solucoes, 10))