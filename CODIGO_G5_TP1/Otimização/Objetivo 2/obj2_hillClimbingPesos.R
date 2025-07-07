# Carregar utilitários
library(mco)
source("otiutils.R")
set.seed(42)

# Ordenar matriz pela coluna (por defeito, a primeira)
col_sort_matrix <- function(M, column = 1) {
  if (is.matrix(M)) {
    I <- sort.int(M[, column], index.return = TRUE)
    M <- M[I$ix, ]
  }
  return(M)
}

# Filtrar soluções não dominadas (paretoFilter)
paretoFilter <- function(M) {
  I <- rep(TRUE, nrow(M))
  for (i in 1:(nrow(M) - 1)) {
    for (j in (i + 1):nrow(M)) {
      if (all(M[i, ] <= M[j, ]) && any(M[i, ] < M[j, ])) I[j] <- FALSE
      if (all(M[j, ] <= M[i, ]) && any(M[j, ] < M[i, ])) I[i] <- FALSE
    }
  }
  res <- M[I, , drop = FALSE]
  if (is.null(dim(res))) res <- matrix(res, ncol = ncol(M))
  return(res)
}

# Definir os limites e a função multiobjetivo
dim_s <- length(calc_upper(pred_sales))
lower <- rep(0, dim_s)
upper <- calc_upper(pred_sales)

obj_fun <- function(x) {
  s2 <- repair(round(x))
  lucro <- eval(s2)
  total_recursos <- sum(s2[1:7]) + sum(matrix(s2[8:28], nrow = 7, byrow = TRUE))
  return(c(lucro, total_recursos))
}

hill_wfun <- function(x, W, K_W = 2/4) {
  res <- obj_fun(x)
  return(K_W * W * res[1] + (1 - W) * res[2])
}

# Algoritmo Hill Climbing com pesos
hill_climbing <- function(W, max_iter = 5000, sigma = 1) {
  K_W <- 2/4
  best <- runif(dim_s, min = lower, max = upper)
  best_score <- hill_wfun(best, W, K_W)
  best_obj <- obj_fun(best)
  
  for (i in 1:max_iter) {
    candidate <- best + rnorm(dim_s, mean = 0, sd = sigma)
    candidate <- pmin(pmax(candidate, lower), upper)
    score <- hill_wfun(candidate, W, K_W)
    if (score < best_score) {
      best <- candidate
      best_score <- score
      best_obj <- obj_fun(candidate)
    }
  }
  return(best_obj)
}

# Correr para vários pesos
WTS <- c(0.1, 0.25, 0.5, 0.75, 0.9)
M_RAW <- t(sapply(WTS, hill_climbing))
colnames(M_RAW) <- c("lucro_neg", "recursos")

# Converter para data.frame e inverter sinal do lucro para POSITIVO
M_PT <- data.frame(
  lucro = -M_RAW[, "lucro_neg"],
  recursos = M_RAW[, "recursos"]
)

# Filtrar soluções não dominadas
M_PT <- paretoFilter(as.matrix(M_PT))
M_PT <- as.data.frame(M_PT)
colnames(M_PT) <- c("lucro", "recursos")

# Ordenar por recursos
M_PT <- M_PT[order(M_PT$recursos), ]

# Calcular hipervolume
ref_pt <- c(max(M_PT$recursos) * 1.1, min(M_PT$lucro) * 0.9)
hv <- dominatedHypervolume(as.matrix(M_PT[, c("recursos", "lucro")]), ref_pt)

# Mostrar frente de Pareto
plot(M_PT$recursos, M_PT$lucro, xlab = "Recursos", ylab = "Lucro", pch = 19,
     main = paste("Frente de Pareto - Hill Climbing com Pesos", round(hv, 2)))
lines(M_PT$recursos, M_PT$lucro, type = "l", col = "gray", lwd = 2)
abline(v = ref_pt[1], col = "gray")
abline(h = ref_pt[2], col = "gray")
points(ref_pt[1], ref_pt[2], col = "gray")

# Mostrar resultados
cat("Soluções Pareto encontradas:\n")
for (i in 1:nrow(M_PT)) {
  cat(i, "| Lucro:", round(M_PT[i, "lucro"], 2), "| Recursos:", round(M_PT[i, "recursos"], 2), "\n")
}

cat("\nHill Climbing com Pesos - Hypervolume:", round(hv, 2), "\n")
cat("Número de soluções Pareto:", nrow(M_PT), "\n")
