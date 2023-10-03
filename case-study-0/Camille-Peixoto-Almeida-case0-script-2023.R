
# Camille Peixoto Almeida 12702259 - CASE 0

# importar a biblioteca
library(tidyverse)

# selecionarr a base de dados
df <- readRDS("H:/Meu Drive/USP/1°Quadri/reof_estat/Estudo de Caso 0 Uso do R-20230322/imdb.rds")

# retirar os espaços nulos (NA)
df <- na.omit(df)

# excluir a coluna pais
df$pais = NULL

# definir a coluna idade
df$idade = 2023 - df$ano

# definir a coluna velho ou novo
df$status = "Novo/Velho"

# Caracterizar  velho/novo idades >= a 50 (velhos) e < 50 (novos)
for (i in 1:2987) {
  if(df$idade[i]>=50){
    df$status[i] = "Velho"
  }
  else{
    df$status[i]="Novo"
  }
}

# contagem de velhos e novos
table(df$status)

# criar uma base de dados nova as novas modificações
write_rds(df,"banco_de_dados_case0_camille")


