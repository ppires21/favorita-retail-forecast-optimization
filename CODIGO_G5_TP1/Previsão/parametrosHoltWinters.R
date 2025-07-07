library(forecast)
library(rminer)

# Carregar dados
d = read.table("sales_clean.csv", header=TRUE, sep=";")

# Configurações
H = 14  # Horizonte de previsão
K = 7   # Frequência semanal
LTR = nrow(d) - H  # Limite para treino

# Parâmetros a testar
alpha_vals = c(0.1, 0.2, 0.3, 0.5, 0.8)
beta_vals = c(TRUE, FALSE)
gamma_vals = c(TRUE)

# Lista de séries
series_list = list(
  "s_d11" = d$s_d11,
  "s_d12" = d$s_d12,
  "s_e11" = d$s_e11,
  "s_e12" = d$s_e12,
  "s_b11" = d$s_b11,
  "s_b12" = d$s_b12
)

# Resultados
results = data.frame(
  Série = character(),
  Alpha = numeric(),
  Beta = logical(),
  Gamma = logical(),
  NMAE = numeric(),
  stringsAsFactors = FALSE
)

# Grid search por série
for (label in names(series_list)) {
  cat("\n---", label, "---\n")
  
  serie_total = series_list[[label]]
  TS = ts(serie_total[1:LTR], frequency=K)
  real = serie_total[(LTR+1):(LTR+H)]
  YR = diff(range(serie_total))  # para normalizar
  
  best_nmae = Inf
  best_model = NULL
  best_params = list()
  
  for (a in alpha_vals) {
    for (b in beta_vals) {
      for (g in gamma_vals) {
        tryCatch({
          modelo = HoltWinters(TS, alpha=a, beta=b, gamma=g)
          pred = forecast(modelo, h=H)$mean
          nmae = mmetric(real, pred, metric="NMAE", val=YR)
          
          if (nmae < best_nmae) {
            best_nmae = nmae
            best_model = modelo
            best_params = list(alpha=a, beta=b, gamma=g)
          }
        }, error=function(e){})
      }
    }
  }
  
  # Guardar resultado
  results = rbind(results, data.frame(
    Série = label,
    Alpha = best_params$alpha,
    Beta = best_params$beta,
    Gamma = best_params$gamma,
    NMAE = round(best_nmae, 3)
  ))
  
  # Mostrar previsões
  pred_final = forecast(best_model, h=H)$mean
  mgraph(real, pred_final, graph="REG", main=paste(label, "- NMAE:", round(best_nmae, 2)),
         col=c("black", "blue"), leg=list(pos="topleft", leg=c("Real", "Previsto")))
}

# Mostrar todos os resultados
print(results)
