# rolling_growing_snaive_print.R
library(rminer)

# 1. Parâmetros fixos
forecast_horizon <- 7    # horizonte de previsão (dias)
seasonality      <- 7    # padrão semanal
increment        <- 7    # incremento da cada janela
Runs             <- 10   # número de iterações

# 2. Carregar dados
sales <- read.csv("sales_clean.csv", sep = ";")
sales$date <- as.Date(sales$date)
sales_cleaned <- sales[1:(nrow(sales) - 3), ]
sales_cols <- c("s_d11", "s_d12", "s_e11", "s_e12", "s_b11", "s_b12")

# 3. Preparar vector para guardar médias de NMAE
res_nmae <- setNames(numeric(length(sales_cols)), sales_cols)

# 4. Loop por cada série
for (col in sales_cols) {
  cat("\n=== SNaive – Série:", col, "===\n")
  serie <- tail(sales_cleaned[[col]], 365)
  d1    <- as.numeric(serie)
  YR    <- diff(range(d1))
  W     <- length(d1) - forecast_horizon - (Runs - 1) * increment
  
  nmae_v <- numeric(Runs)
  for (b in 1:Runs) {
    H <- holdout(d1,
                 ratio     = forecast_horizon,
                 mode      = "incremental",
                 iter      = b,
                 window    = W,
                 increment = increment)
    A <- d1[H$ts]
    P <- d1[H$ts - seasonality]
    nmae_v[b] <- mmetric(A, P, metric="NMAE", val=YR)
    cat(sprintf("Iter %2d – NMAE = %.3f\n", b, nmae_v[b]))
  }
  
  nmae_mean <- mean(nmae_v)
  res_nmae[col] <- nmae_mean
  cat(sprintf(">>> Média NMAE SNaive para %s: %.3f\n", col, nmae_mean))
}

# 5. Mostrar no ecrã a tabela de resultados
df <- data.frame(
  Série       = names(res_nmae),
  NMAE_SNaive = round(unname(res_nmae), 3)
)
cat("\n--- Tabela de NMAE médio por série (SNaive) ---\n")
print(df)