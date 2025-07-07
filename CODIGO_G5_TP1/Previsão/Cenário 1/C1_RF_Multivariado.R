library(rminer)       # fit, holdout, mmetric, mgraph, CasesSeries, lforecastm
library(randomForest) # Para Random Forest
source("C:/Users/joaoa/OneDrive/Documentos/Rseries/multi-utils.R")  # contêm lforecastm(), mas vamos sobrescrever mfit()

## -----------------------------------------------------------------------------
##  Função mfit adaptada para aceitar ... e passar ntree/mtry ao fit()
## -----------------------------------------------------------------------------
mfit_rf <- function(mtr, model, VINP, exogen = NULL, ...) {
  nmodels <- ncol(mtr)
  # Determina lag máximo
  lagmax <- 1
  for(i in 1:nmodels) for(j in 1:nmodels)
    lagmax <- max(lagmax, VINP[[i]][[j]])
  
  # Gera todos os datasets de lags
  mdata <- vector("list", nmodels)
  for(i in 1:nmodels) mdata[[i]] <- CasesSeries(mtr[,i], 1:lagmax)
  
  # Pré-processa exógenas se houver
  if(!is.null(exogen)) {
    NR <- nrow(mtr)
    if(is.vector(exogen) && NR == length(exogen))
      exogen <- as.data.frame(exogen[(lagmax+1):NR])
    else if(NR == nrow(exogen))
      exogen <- as.data.frame(exogen[(lagmax+1):NR,])
  }
  
  # Constroi mdata2: um data.frame por modelo
  mdata2 <- vector("list", nmodels)
  for(i in 1:nmodels) {
    D <- NULL
    for(j in 1:nmodels) {
      inames <- paste0("lag", VINP[[i]][[j]])
      block  <- mdata[[j]][, inames, drop=FALSE]
      colnames(block) <- paste0("x", j, rev(inames))
      D <- if(is.null(D)) block else cbind(D, block)
    }
    if(!is.null(exogen)) D <- cbind(D, exogen, y = mdata[[i]]$y)
    else               D <- cbind(D,          y = mdata[[i]]$y)
    mdata2[[i]] <- D
  }
  
  # Treina um Random Forest por série, propagando ... (ntree, mtry, etc.)
  mmodels <- vector("list", nmodels)
  for(i in 1:nmodels) {
    mmodels[[i]] <- fit(
      y ~ .,
      data  = mdata2[[i]],
      model = model,
      ...
    )
  }
  
  list(mdata = mdata2, mmodels = mmodels, vinp = VINP)
}

## -----------------------------------------------------------------------------
##  Função auxiliar de plotagem (RF multivariado)
## -----------------------------------------------------------------------------
fshow6_rf <- function(Y, Pred, series_names) {
  par(mfrow = c(2, 3))
  for(i in seq_along(series_names)) {
    mgraph(Y[,i], Pred[[i]],
           graph = "REG",
           main  = paste(series_names[i], "- Multivariado RF"),
           Grid  = 10,
           col   = c("black","darkgreen"),
           leg   = list(pos="topleft", leg=c("target","previsto")))
  }
}

# --- 1. Parâmetros e colunas ---
series_cols  <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")
horizon      <- 7
lag_max      <- 365
th_lag       <- 0.2
min_pairs    <- 2
min_own_lags <- 3

# --- 2. Carregamento e holdout ---
sales <- read.csv("C:/Users/joaoa/OneDrive/Documentos/Rstudio/sales_clean.csv", sep=";")
sales$date <- as.Date(sales$date)
cdata <- as.matrix(sales[, series_cols])
hd    <- holdout(cdata[,1], ratio = horizon, mode = "order")
mtr   <- ts(cdata[hd$tr, ], frequency = 1)
Y     <-     cdata[hd$ts, ]
n_series <- ncol(mtr)

# --- 3. VINP por correlação (idem) ---
VINP_corr <- vector("list", length = n_series)
names(VINP_corr) <- series_cols
for(i in 1:n_series) {
  VINP_corr[[i]] <- vector("list", length = n_series)
  names(VINP_corr[[i]]) <- series_cols
  for(j in 1:n_series) {
    cors <- sapply(1:lag_max, function(k) {
      x <- mtr[(k+1):nrow(mtr), i]
      y <- mtr[1:(nrow(mtr)-k), j]
      cor(x, y, use = "complete.obs")
    })
    sel_lags <- which(abs(cors) > th_lag)
    if(length(sel_lags) < min_pairs) {
      ord <- order(abs(cors), decreasing = TRUE)
      sel_lags <- ord[1:min_pairs]
    }
    if(i == j) sel_lags <- sort(unique(c(sel_lags, 1:min_own_lags)))
    VINP_corr[[i]][[j]] <- sel_lags
  }
}

# --- 4. Treino multivariado com RF ---
MNN_rf <- mfit_rf(
  mtr    = mtr,
  model  = "randomForest",
  VINP   = VINP_corr,
  ntree  = 100,   # agora é 100
  mtry   = 6      # agora é 6
)


# --- 5. Previsão multi-step ---
Pred_rf <- lforecastm(
  model = MNN_rf,
  h     = horizon
)

# --- 6. Avaliação e gráficos ---
fshow6_rf(Y, Pred_rf, series_cols)

# Tabela final de métricas
resultados_rf <- data.frame(
  Serie = series_cols,
  MAE   = sapply(1:n_series, function(i)
    mmetric(Y[,i], Pred_rf[[i]], metric="MAE")),
  NMAE  = sapply(1:n_series, function(i)
    mmetric(Y[,i], Pred_rf[[i]], metric="NMAE", val=diff(range(Y[,i])))),
  RMSE  = sapply(1:n_series, function(i)
    mmetric(Y[,i], Pred_rf[[i]], metric="RMSE")),
  RRSE  = sapply(1:n_series, function(i)
    mmetric(Y[,i], Pred_rf[[i]], metric="RRSE"))
)
print(resultados_rf)
