# spea2_algo.R
# Módulo para SPEA-2 e geração do Pareto-front

# 0) Dependências
if (!requireNamespace("emoa", quietly = TRUE)) install.packages("emoa")
if (!requireNamespace("ecr",  quietly = TRUE)) install.packages("ecr")
if (!requireNamespace("mco",  quietly = TRUE)) install.packages("mco")

library(emoa)
library(ecr)
library(mco)

# 1) Função auxiliar para ordenar pela coluna desejada
col_sort_matrix <- function(M, column = 2) {
  if (is.matrix(M)) {
    I  <- order(M[, column])
    M  <- M[I, , drop = FALSE]
  }
  M
}

# 2) Definição da função multi-objetivo (–lucro, uso de recursos)
.obj_spea2 <- function(x) {
  # x: vetor numérico de comprimento dim_s
  s2 <- repair(round(x))        # aplica restrições
  lucro <- eval(s2)              # invoca a função de lucro (usa pred_sales e weekend)
  # uso total de recursos = W (7 dias) + V (7×3)
  recursos <- sum(s2[1:7]) + sum(matrix(s2[8:28], nrow = 7, ncol = 3, byrow = TRUE))
  c(-lucro, recursos)
}

# 3) Implementação manual do SPEA-2
spea2_manual <- function(fn, lower, upper,
                         n.pop  = 100L,
                         n.arch = 100L,
                         n.gen  = 49L) {
  n.var <- length(lower)
  # População inicial
  P   <- replicate(n.pop, runif(n.var, lower, upper), simplify = FALSE)
  V_P <- t(sapply(P, fn))
  A   <- P; V_A <- V_P
  
  for (gen in seq_len(n.gen)) {
    U   <- c(P, A)
    V_U <- rbind(V_P, V_A)
    M   <- nrow(V_U)
    
    # Força S e fitness R
    S <- vapply(seq_len(M), function(i) sum(apply(V_U, 1, function(vj) dominates(V_U[i,], vj))), integer(1))
    R <- vapply(seq_len(M), function(i) sum(S[apply(V_U, 1, function(vj) dominates(vj, V_U[i,]))]), integer(1))
    
    # Densidade
    k    <- floor(sqrt(M))
    mins <- apply(V_U, 2, min); maxs <- apply(V_U, 2, max)
    Vn   <- sweep(sweep(V_U, 2, mins, "-"), 2, maxs - mins, "/")
    Dmat <- as.matrix(dist(Vn))
    Dens <- vapply(seq_len(M), function(i) {
      dists <- sort(Dmat[i, -i]); 1 / (dists[k] + 2)
    }, numeric(1))
    
    Fit <- R + Dens
    
    # Atualiza arquivo A
    idx  <- order(Fit)
    pick <- idx[seq_len(min(n.arch, length(idx)))]
    A    <- U[pick]
    V_A  <- V_U[pick, , drop = FALSE]
    
    # Seleção por torneio e variação
    Fit_A <- Fit[pick]
    Mpool <- replicate(n.pop, {
      cands <- sample(length(A), 2)
      if (Fit_A[cands[1]] < Fit_A[cands[2]]) A[[cands[1]]] else A[[cands[2]]]
    }, simplify = FALSE)
    
    Off <- vector("list", n.pop)
    for (j in seq(1, n.pop, by = 2)) {
      p1 <- Mpool[[j]]; p2 <- Mpool[[min(j+1, n.pop)]]
      alpha <- runif(n.var)
      c1 <- alpha*p1 + (1-alpha)*p2
      c2 <- alpha*p2 + (1-alpha)*p1
      # mutação (σ = 10% do intervalo)
      sigma <- 0.1 * (upper - lower)
      c1 <- pmin(upper, pmax(lower, c1 + rnorm(n.var, 0, sigma)))
      c2 <- pmin(upper, pmax(lower, c2 + rnorm(n.var, 0, sigma)))
      Off[[j]] <- c1
      if (j+1 <= n.pop) Off[[j+1]] <- c2
    }
    
    P   <- Off
    V_P <- t(sapply(P, fn))
  }
  
  list(archive = A, objectives = V_A)
}

# 4) Função principal para a interface
optimize_spea2 <- function(pred_sales, weekend_vec,
                           n.pop  = 100L,
                           n.arch = 100L,
                           n.gen  = 49L) {
  # preparar ambiente
  assign("pred_sales", pred_sales, envir = .GlobalEnv)
  assign("weekend",    weekend_vec, envir = .GlobalEnv)
  
  dim_s <- length(calc_upper(pred_sales))
  lower <- rep(0, dim_s)
  upper <- calc_upper(pred_sales)
  
  # correr SPEA-2
  res <- spea2_manual(.obj_spea2, lower, upper, n.pop, n.arch, n.gen)
  pf  <- res$objectives
  
  # filtrar não-dominados e ordenar
  pf_nd       <- paretoFilter(pf)
  P_nd_sorted <- col_sort_matrix(pf_nd, 2)
  
  recursos <- P_nd_sorted[,2]
  lucros   <- -P_nd_sorted[,1]
  
  list(recursos = recursos, lucros = lucros)
}
