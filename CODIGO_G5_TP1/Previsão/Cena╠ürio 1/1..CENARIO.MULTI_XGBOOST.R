library(rminer)     # fit, holdout, mmetric, mgraph, CasesSeries, lforecastm
library(xgboost)    # garante disponibilidade do XGBoost
source("multi-utils.R")  # mfit() e lforecastm()

## -----------------------------------------------------------------------------
##  Função auxiliar de plotagem para 6 séries
## -----------------------------------------------------------------------------
fshow6 <- function(Y, Pred, series_names, method) {
  par(mfrow = c(2, 3))   # 2 linhas × 3 colunas de gráficos
  for (i in 1:length(series_names)) {
    mae  <- round(mmetric(Y[,i], Pred[[i]], metric="MAE"), 1)
    nmae <- round(mmetric(Y[,i], Pred[[i]], metric="NMAE", val=diff(range(Y[,i]))), 1)
    rmse <- round(mmetric(Y[,i], Pred[[i]], metric="RMSE"), 1)
    rrse <- round(mmetric(Y[,i], Pred[[i]], metric="RRSE"), 1)
    main <- paste(series_names[i], "- Multivariado XGBoost")
    mgraph(Y[,i], Pred[[i]],
           graph = "REG",
           main  = main,
           Grid  = 10,
           col   = c("black","blue"),
           leg   = list(pos="topleft", leg=c("target","previsto")))
  }
}

# --- 1. Definição de Parâmetros ---
series_cols  <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")
horizon      <- 7     # passos à frente
lag_max      <- 365     # máximo lag
th_lag       <- 0.2    # limiar de correlação absoluta
min_pairs    <- 2      # mínimo de lags por par (i,j)
min_own_lags <- 3      # sempre incluir estes lags próprios

# --- 2. Carregamento e separação dos dados ---
sales <- read.csv("sales_clean.csv", sep=";")
sales$date <- as.Date(sales$date)

# usa a primeira série para definir holdout (equivalente ao ficheiro do order split)
cdata <- as.matrix(sales[, series_cols])
hd    <- holdout(cdata[,1], ratio=horizon, mode="order")

mtr <- ts(cdata[hd$tr, ], frequency = 1)  # treino em objeto ts
Y   <-     cdata[hd$ts, ]                  # valores reais para teste

n_series <- ncol(mtr)

# --- 3. Construção da VINP baseada em correlação ---
VINP_corr <- vector("list", length = n_series)
names(VINP_corr) <- series_cols

for (i in 1:n_series) {
  VINP_corr[[i]] <- vector("list", length = n_series)
  names(VINP_corr[[i]]) <- series_cols
  
  for (j in 1:n_series) {
    # correlação entre série i em t e série j em t–k, k = 1…lag_max
    cors <- sapply(1:lag_max, function(k) {
      x <- mtr[(k+1):nrow(mtr), i]
      y <- mtr[1:(nrow(mtr)-k), j]
      cor(x, y, use = "complete.obs")
    })
    # lags acima do limiar
    sel_lags <- which(abs(cors) > th_lag)
    # garante pelo menos min_pairs lags
    if (length(sel_lags) < min_pairs) {
      ord      <- order(abs(cors), decreasing = TRUE)
      sel_lags <- ord[1:min_pairs]
    }
    # para própria série, inclui sempre os primeiros min_own_lags
    if (i == j) {
      sel_lags <- sort(unique(c(sel_lags, 1:min_own_lags)))
    }
    VINP_corr[[i]][[j]] <- sel_lags
  }
}

#ver resumo das seleções
print("Seleção de lags (VINP_corr):")
print(VINP_corr)

# --- 4. Treino do modelo multivariado XGBoost ---
MNN <- mfit(
  mtr   = mtr,
  model = "xgboost",
  VINP  = VINP_corr
)

# --- 5. Previsão multi‑step à frente ---
Pred <- lforecastm(
  model = MNN,
  h     = horizon
)

# --- 6. Avaliação e gráficos ---
fshow6(Y, Pred, series_cols, "XGBoost")

# mostrar tabela de métricas
resultados <- data.frame(
  Serie = series_cols,
  MAE   = sapply(1:n_series, function(i) mmetric(Y[,i], Pred[[i]], metric="MAE")),
  NMAE  = sapply(1:n_series, function(i) mmetric(Y[,i], Pred[[i]], metric="NMAE", val=diff(range(Y[,i])))),
  RMSE  = sapply(1:n_series, function(i) mmetric(Y[,i], Pred[[i]], metric="RMSE")),
  RRSE  = sapply(1:n_series, function(i) mmetric(Y[,i], Pred[[i]], metric="RRSE"))
)
print(resultados)
