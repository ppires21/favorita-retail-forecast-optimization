library(forecast)
library(rminer)

# 1. Carregar e filtrar os dados
d = read.table("sales_clean.csv", header=TRUE, sep=";")
d$date = as.Date(d$date)
ultima_data = max(d$date)
inicio_ano = ultima_data - 364
d = d[d$date >= inicio_ano & d$date <= ultima_data, ]

# 2. Sazonalidade e horizonte de previsão
K = 7
H = 7

# 3. Lista de séries
series_list = list(
  "s_d11" = d$s_d11,
  "s_d12" = d$s_d12,
  "s_e11" = d$s_e11,
  "s_e12" = d$s_e12,
  "s_b11" = d$s_b11,
  "s_b12" = d$s_b12
)

# 4. Parâmetros HW por modo (growing e rolling)
hw_params_growing = list(
  "s_b11" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10),
  "s_b12" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10),
  "s_d11" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10),
  "s_d12" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10),
  "s_e11" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10),
  "s_e12" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10)
)

hw_params_rolling = list(
  "s_b11" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10),
  "s_b12" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10),
  "s_d11" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10),
  "s_d12" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10),
  "s_e11" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10),
  "s_e12" = list(alpha=0.3, beta=FALSE, gamma=TRUE, lag=15, increment=7,  runs=10)
)

# 5. Função de avaliação HW
avaliar_hw <- function(series, label, modo="rolling", params_list) {
  cat("\n---", toupper(modo), ":", label, "---\n")
  series = as.numeric(series)
  p = params_list[[label]]
  
  mae = rmse = nmae = rrse = numeric(p$runs)
  YR = diff(range(series))
  
  for (b in 1:p$runs) {
    HLD = holdout(series, ratio=H, mode=modo, iter=b, window=p$lag, increment=p$increment)
    dtr = ts(series[HLD$tr], frequency=K)
    
    M = suppressWarnings(HoltWinters(dtr, alpha=p$alpha, beta=p$beta, gamma=p$gamma))
    Pred = forecast(M, h=length(HLD$ts))$mean[1:H]
    
    mae[b]  = mmetric(series[HLD$ts], Pred, metric="MAE")
    rmse[b] = mmetric(series[HLD$ts], Pred, metric="RMSE")
    nmae[b] = mmetric(series[HLD$ts], Pred, metric="NMAE", val=YR)
    rrse[b] = mmetric(series[HLD$ts], Pred, metric="RRSE")
    
    cat("Iter:", b,
        "| MAE:", round(mae[b], 2),
        "| NMAE:", round(nmae[b], 3),
        "| RMSE:", round(rmse[b], 2),
        "| RRSE:", round(rrse[b], 3), "\n")
  }
  
  return(data.frame(
    Serie = label,
    Metodo = toupper(modo),
    MAE = round(median(mae), 2),
    NMAE = round(median(nmae), 3),
    RMSE = round(median(rmse), 2),
    RRSE = round(median(rrse), 3)
  ))
}

# 6. Avaliar todas as séries
resultados = data.frame()

for (nome in names(series_list)) {
  resultados = rbind(resultados, avaliar_hw(series_list[[nome]], nome, "rolling", hw_params_rolling))
  resultados = rbind(resultados, avaliar_hw(series_list[[nome]], nome, "growing", hw_params_growing))
}

# 7. Guardar resultados
write.csv(resultados, "resultados_hw.csv", row.names=FALSE)
cat("\n Resultados guardados em 'resultados_hw.csv'\n")
