# Carregar os dados
dados <- read.csv("sales.csv", sep = ";", header = TRUE)

# Ver estrutura do dataset
str(dados)

# Ver primeiros valores de cada coluna
head(dados)

# Ver tipos de dados resumidos
sapply(dados, class)

# Ver classes únicas (para colunas com múltiplas classes possíveis)
lapply(dados, class)

# Ver primeiros 5 valores de cada coluna (resumo rápido para a tabela)
lapply(dados, function(x) head(x, 5))