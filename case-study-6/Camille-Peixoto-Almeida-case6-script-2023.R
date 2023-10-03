# Camille Peixoto Almeida 12702259 - CASE 0

# importar a biblioteca
library(tidyverse)

# selecionarr a base de dados
df <- readRDS("H:/Meu Drive/USP/semestres_passados/1°Quadri2023/reof_estat/Estudo de Caso 5 - Teste de Hipóteses I-20230510/Case5/bundesliga.rds")

df_HomeDortmund <- subset(df,df$HomeTeam == "Dortmund")
df_AwayDortmund <- subset(df, df$AwayTeam == "Dortmund")

# o Dortmund sofre 1 gol em média todo jogo como mandante - verdade atual
# provar que a quantidade de gols sofrida pelo time em jogos como mandante é diferente de 1 gol.

# TESTE DE HIPÓTESE 
# H0: u = 2.6
# H1: u diferente de 2.6

media_gols_tomados <- mean(df_HomeDortmund$FullTimeAwayGoals) 

dev_pad_populacional <- 0.982
media0 <- 1

n <- 170
z <- 1.96
  
x1crit <- media0 - z*dev_pad_populacional/(n)^0.5
x2crit <- media0 + z*dev_pad_populacional/(n)^0.5

hist(df_HomeDortmund$FullTimeAwayGoals,
     breaks = 10,
     freq = T,
     col = "yellow",
     ylab = "Frequência",
     xlab = "Gols tomados em casa",
     main = "Histograma de médias amostrais")

# como a média amostral vale 1 e pertence ao intervalo [x1crit, x2crit] não rejeitamos a hipótese inicial
###############################################################
# desvio padrão desconhecido

media_gols_tomados <- mean(df_HomeDortmund$FullTimeAwayGoals) 
dev_pad_gols_tomados <- sd(df_HomeDortmund$FullTimeAwayGoals)

media0 <- 1

n <- 170
t <- 1.9741

x1crit_desc <- media0 - t*dev_pad_gols_tomados/(n)^0.5
x2crit_desc <- media0 + t*dev_pad_gols_tomados/(n)^0.5

# como a média amostral vale 1 e pertence ao intervalo [x1crit, x2crit] 
# não rejeitamos a hipótese inicial


#qnorm(0.05)
#qt(0.025, 169)
#qchiq(0.9, df = 169)
######################################

# TESTE DE HIPÓTESE
# H0: var0 = 1.5^2
# H1: var1 > var0

var_populacional <- 1.5^2
var_amostral <- var(df_HomeDortmund$FullTimeAwayGoals)

b10porcento <- 192.948
b5porcento <- 200.334

Scrit_aoquadrado <- b5porcento*var_populacional/(n-1)

# como Scrit_quadrado > var_amostral não podemos rejeitar H0

#considerando 5% de significância



























