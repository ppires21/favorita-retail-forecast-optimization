library(rminer)
library(forecast)
library(seastests)

# Carregar a base de dados 
d = read.table("sales_clean.csv", header=TRUE, sep=";")

# Definir o horizonte de previsão (H = 14 dias)
H = 7  # Uma semanas de previsões
K = 7   # Frequência semanal (dados diários)

# Separar os dados de treino e teste
LTR = nrow(d) - H  # Dados de treino até o ponto final menos o horizonte de previsão

# Criar séries temporais para cada produto e loja
TS_d11 = ts(d$s_d11[1:LTR], frequency=K)
TS_d12 = ts(d$s_d12[1:LTR], frequency=K)
TS_e11 = ts(d$s_e11[1:LTR], frequency=K)
TS_e12 = ts(d$s_e12[1:LTR], frequency=K)
TS_b11 = ts(d$s_b11[1:LTR], frequency=K)
TS_b12 = ts(d$s_b12[1:LTR], frequency=K)

# Função para mostrar todas as métricas
avaliar_modelo <- function(label, real, pred) {
  YR = diff(range(real))  # Para o cálculo do NMAE
  cat("\n---", label, "---\n")
  cat("MAE  :", round(mmetric(real, pred, metric="MAE"), 3), "\n")
  cat("NMAE :", round(mmetric(real, pred, metric="NMAE", val=YR), 3), "\n")
  cat("RMSE :", round(mmetric(real, pred, metric="RMSE"), 3), "\n")
  cat("RRSE :", round(mmetric(real, pred, metric="RRSE"), 3), "\n")
  
  # Visualização gráfica com MAE no título
  main = paste(label, "MAE:", round(mmetric(real, pred, metric="MAE"), 1))
  mgraph(real, pred, graph="REG", main=main, Grid=10, 
         col=c("black", "blue"), leg=list(pos="topleft", leg=c("Real", paste(label, "Prev."))))
}

# Aplicar o modelo Holt-Winters a cada série temporal
HW_d11 = HoltWinters(TS_d11, alpha=0.3, beta=FALSE, gamma=TRUE)
HW_d12 = HoltWinters(TS_d12, alpha=0.1, beta=FALSE, gamma=TRUE)
HW_e11 = HoltWinters(TS_e11, alpha=0.5, beta=FALSE, gamma=TRUE)
HW_e12 = HoltWinters(TS_e12, alpha=0.1, beta=FALSE, gamma=TRUE)
HW_b11 = HoltWinters(TS_b11, alpha=0.3, beta=FALSE, gamma=TRUE)
HW_b12 = HoltWinters(TS_b12, alpha=0.5, beta=FALSE, gamma=TRUE)


# Gerar previsões para os próximos H dias
F_d11 = forecast(HW_d11, h=H)
F_d12 = forecast(HW_d12, h=H)
F_e11 = forecast(HW_e11, h=H)
F_e12 = forecast(HW_e12, h=H)
F_b11 = forecast(HW_b11, h=H)
F_b12 = forecast(HW_b12, h=H)

# Comparar com os valores reais e calcular métricas
avaliar_modelo("HW_d11", d$s_d11[(LTR+1):(LTR+H)], F_d11$mean)
avaliar_modelo("HW_d12", d$s_d12[(LTR+1):(LTR+H)], F_d12$mean)
avaliar_modelo("HW_e11", d$s_e11[(LTR+1):(LTR+H)], F_e11$mean)
avaliar_modelo("HW_e12", d$s_e12[(LTR+1):(LTR+H)], F_e12$mean)
avaliar_modelo("HW_b11", d$s_b11[(LTR+1):(LTR+H)], F_b11$mean)
avaliar_modelo("HW_b12", d$s_b12[(LTR+1):(LTR+H)], F_b12$mean)
