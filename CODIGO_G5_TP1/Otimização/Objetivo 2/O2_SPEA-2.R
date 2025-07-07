# SPEA2_MANUAL_MULTI_ITER_PARETO_SEMANA.R

if (!requireNamespace("emoa", quietly = TRUE)) install.packages("emoa")
if (!requireNamespace("ecr", quietly = TRUE)) install.packages("ecr")
if (!requireNamespace("mco", quietly = TRUE)) install.packages("mco")
library(emoa)
library(ecr)
library(mco)

col_sort_matrix <- function(M, column = 1) {
  if (is.matrix(M)) {
    I <- order(M[, column])
    M <- M[I, , drop = FALSE]
  }
  M
}

source("otiutils.R")

set.seed(42)

# 1) Carregar previsões do CSV
df_pred <- read.csv("ITER_Sales_prev.csv")
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")
Runs <- length(unique(df_pred$Iteracao))

# 2) Reconstruir lista de previsões
pred_list <- vector("list", Runs)
for (b in seq_len(Runs)) {
  sub            <- df_pred[df_pred$Iteracao == b, sales_cols]
  pred_list[[b]] <- as.matrix(sub)
}

# 3) SPEA2 manual igual ao original
spea2_manual <- function(obj_fun, lower, upper, n.pop = 100L, n.arch = 100L, n.gen = 49L) {
  n.var <- length(lower)
  P <- replicate(n.pop, runif(n.var, min = lower, max = upper), simplify = FALSE)
  V_P <- t(sapply(P, obj_fun))
  A <- P
  V_A <- V_P
  for (gen in seq_len(n.gen)) {
    U <- c(P, A)
    V_U <- rbind(V_P, V_A)
    M <- nrow(V_U)
    S <- integer(M)
    for (i in seq_len(M)) {
      S[i] <- sum(apply(V_U, 1, function(vj) dominates(V_U[i,], vj)))
    }
    R <- integer(M)
    for (i in seq_len(M)) {
      R[i] <- sum(S[apply(V_U, 1, function(vj) dominates(vj, V_U[i,]))])
    }
    k <- floor(sqrt(M))
    mins <- apply(V_U, 2, min); maxs <- apply(V_U, 2, max)
    Vn <- sweep(sweep(V_U, 2, mins, "-"), 2, maxs - mins, "/")
    Dmat <- as.matrix(dist(Vn))
    Dens <- numeric(M)
    for (i in seq_len(M)) {
      dists <- sort(Dmat[i, -i])
      Dens[i] <- 1 / (dists[k] + 2)
    }
    Fit <- R + Dens
    idx <- order(Fit)
    pick <- idx[seq_len(min(n.arch, length(idx)))]
    A <- U[pick]
    V_A <- V_U[pick, , drop = FALSE]
    Fit_A <- Fit[pick]
    Mpool <- vector("list", n.pop)
    for (j in seq_len(n.pop)) {
      cands <- sample(length(A), 2)
      winner <- if (Fit_A[cands[1]] < Fit_A[cands[2]]) cands[1] else cands[2]
      Mpool[[j]] <- A[[winner]]
    }
    Off <- vector("list", n.pop)
    for (j in seq(1, n.pop, by = 2)) {
      p1 <- Mpool[[j]]; p2 <- Mpool[[min(j+1, n.pop)]]
      alpha <- runif(n.var)
      c1 <- alpha * p1 + (1 - alpha) * p2
      c2 <- alpha * p2 + (1 - alpha) * p1
      c1 <- pmin(upper, pmax(lower, c1 + rnorm(n.var, 0, 0.1 * (upper - lower))))
      c2 <- pmin(upper, pmax(lower, c2 + rnorm(n.var, 0, 0.1 * (upper - lower))))
      Off[[j]] <- c1
      if (j + 1 <= n.pop) Off[[j+1]] <- c2
    }
    P <- Off
    V_P <- t(sapply(P, obj_fun))
  }
  list(archive = A, objectives = V_A)
}

# 4) Loop por semana: SPEA2 igual ao original e plot igual ao original
for (i in seq_len(Runs)) {
  cat(sprintf("Semana %2d/%d — SPEA2...\n", i, Runs))
  pred_sales <- pred_list[[i]] # <- variável global para otiutils.R
  dim_s <- length(calc_upper(pred_sales))
  lower <- rep(0, dim_s)
  upper <- calc_upper(pred_sales)
  
  obj_fun <- function(x) {
    s2 <- repair(round(x))
    lucro <- eval(s2)
    total_recursos <- sum(s2[1:7]) + sum(matrix(s2[8:28], nrow = 7, ncol = 3, byrow = TRUE))
    c(-lucro, total_recursos)
  }
  
  res <- spea2_manual(obj_fun, lower, upper,
                      n.pop  = 100L,
                      n.arch = 100L,
                      n.gen  = 49L)
  
  pf <- res$objectives
  pf_nd <- mco::paretoFilter(pf)
  P_nd_sorted <- col_sort_matrix(pf_nd, 2)
  recursos <-  P_nd_sorted[, 2]
  lucros   <- -P_nd_sorted[, 1]
  
  # Plot por semana
  plot(recursos, lucros,
       type  = "b",
       pch   = 16,
       xlab  = "Uso de Recursos",
       ylab  = "Lucro (EUR)",
       main  = sprintf("Frente de Pareto — SPEA-2 — Semana %d", i))
  
  # Tabela por semana
  solucoes <- data.frame(
    Lucro    = round(lucros,   2),
    Recursos = recursos
  )
  print(head(solucoes, 10))
}