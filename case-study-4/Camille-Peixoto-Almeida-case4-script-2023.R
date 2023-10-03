#################### Camille Peixoto Almeida 12702259 #######################
# importação de bibliotecas
library(tidyverse)
library(ggplot2)

# selecionar a base de dados
df <- readRDS("cripto.rds")

# retirar os espacos nulos (NA)
df <- na.omit(df)

#embaralhar o conjunto de dados
df_embaralhado <- df[sample(1:nrow(df)), ]

#criei 3 banco de dados separados para cada moeda
df_Dogecoin <- subset(df, df$name == "Dogecoin")
df_Stellar <- subset(df,df$name == "Stellar")
df_XRP <- subset(df,df$name == "XRP")

# criei coluna chamada retorno para cada um dos dados para que o dado da 
# dogecoin não seja considerado no retorno da stellar e o dado da stellar não 
# atrapalhe o retorno da XRP

df_Dogecoin$retornoDogecoin = 1
for (i in 2:1811) {
  df_Dogecoin$retornoDogecoin[i] <- log(df_Dogecoin$close[i]/df_Dogecoin$close[i-1], base = exp(1))
}

df_Stellar$retornoStellar = 1
for (i in 2:1578) {
  df_Stellar$retornoStellar[i] <- log(df_Stellar$close[i]/df_Stellar$close[i-1], base = exp(1))
}

df_XRP$retornoXRP = 1
for (i in 2:1944) {
  df_XRP$retornoXRP[i] <- log(df_XRP$close[i]/df_XRP$close[i-1], base = exp(1))
}


# medias amostrais dos retornos de cada moeda

media_doge <- mean(df_Dogecoin$retornoDogecoin)
media_ste <- mean(df_Stellar$retornoStellar)
media_XRP <- mean(df_XRP$retornoXRP)

# DESVIOS PADRÃO CONHECIDOS - NORMAL

# intervalo de confiança
sigma_doge <- 0.02
sigma_ste <- 0.015
sigma_XRP <- 0.012


# 95 % de confiança
z95<- 1.96
# 90% de confiança
z90 <- 1.645
# 99% de confiança
z99 <- 2.575

z <- z99

ndoge <- 1811
extremoD_doge <- media_doge + z*sigma_doge/(ndoge)^0.5
extremoE_doge <- media_doge - z*sigma_doge/(ndoge)^0.5

nste<- 1578
extremoD_ste <- media_ste + z*sigma_ste/(nste)^0.5
extremoE_ste <- media_ste - z*sigma_ste/(nste)^0.5

nXRP <- 1944
extremoD_XRP <- media_XRP + z*sigma_XRP/(nXRP)^0.5
extremoE_XRP <- media_XRP - z*sigma_XRP/(nXRP)^0.5

#DESVIOS PADRÃO DESCONHECIDOS - T-STUDENT

desv_amostral_doge <- sd(df_Dogecoin$retornoDogecoin)
desv_amostral_ste <- sd(df_Stellar$retornoStellar)
desv_amostral_XRP <- sd(df_XRP$retornoXRP)

# 95 % de confiança
t95<- 1.96
# 90% de confiança
t90 <- 1.645
# 99% de confiança
t99 <- 2.575

t <- t95

ndoge <- 1811
extremoD_doge_desc <- media_doge + t*desv_amostral_doge/(ndoge)^0.5
extremoE_doge_desc <- media_doge - t*desv_amostral_doge/(ndoge)^0.5

nste <- 1578
extremoD_ste_desc <- media_ste + t*desv_amostral_ste/(nste)^0.5
extremoE_ste_desc <- media_ste - t*desv_amostral_ste/(nste)^0.5

nXRP <- 1944
extremoD_XRP_desc <- media_XRP + t*desv_amostral_XRP/(nXRP)^0.5
extremoE_XRP_desc <- media_XRP - t*desv_amostral_XRP/(nXRP)^0.5

###############################################################################
# Intervalo de Confiança para Dois Parâmetros : o intervalo de confiança 
#para a diferença da média dos retornos entre as moedas XRP e Stellar

# sigmas conhecidos
diferenca_media <- media_ste - media_XRP
sigma_XRP_part2 <- 0.02
sigma_ste_part2 <- 0.012

extremoD_dif <- diferenca_media + z95*(((sigma_XRP_part2)^2)/nXRP+((sigma_ste_part2)^2)/nste)^0.5
extremoE_dif <- diferenca_media - z95*(((sigma_XRP_part2)^2)/nXRP+((sigma_ste_part2)^2)/nste)^0.5

var_XRP <- var(df_XRP$retornoXRP)
var_ste <- var(df_Stellar$retornoStellar)

# sigmas desconhecidos e iguais

Sp <- (((nste-1)*var_ste+(nXRP -1)*var_XRP)/(nste + nXRP - 2))^0.5

extremoD_dif_desc_iguais <- diferenca_media + Sp*t*(nste^(-1)+ nXRP^(-1))^0.5
extremoE_dif_desc_iguais <- diferenca_media - Sp*t*(nste^(-1)+ nXRP^(-1))^0.5


# sigmas desconhecidos e não necessariamente iguais

extremoD_dif_desc_diferentes <- diferenca_media + t*(var_ste/nste+ var_XRP/nXRP)^0.5
extremoE_dif_desc_diferentes <- diferenca_media - t*(var_ste/nste+ var_XRP/nXRP)^0.5































