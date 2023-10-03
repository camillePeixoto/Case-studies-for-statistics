# Camille Peixoto Almeida 12702259 - CASE 7

# importar a biblioteca
library(tidyverse)

# selecionar a base de dados
df <- readRDS("H:/Meu Drive/USP/semestres_passados/1°Quadri2023/reof_estat/Estudo de Caso 5 - Teste de Hipóteses I-20230510/Case5/bundesliga.rds")

df_HomeDortmund <- subset(df,df$HomeTeam == "Dortmund")
df_AwayDortmund <- subset(df, df$AwayTeam == "Dortmund")

# PARTE 1: TESTE DE HIPÓTESES 2 PARÂMETROS - MÉDIA
# PARA DESVIOS PADRÃO POPULACIONAIS CONHECIDOS
# H0: média sofridos em casa = média sofridos fora de casa
# H1: média sofridos em casa DIFERENTE média sofridos fora de casa

media_tomados_emCasa <- mean(df_HomeDortmund$FullTimeAwayGoals)
media_tomados_foraDeCasa <- mean(df_AwayDortmund$FullTimeHomeGoals)

diferenca_media <- media_tomados_foraDeCasa - media_tomados_emCasa

desv_pad_pop_tomados_emCasa <- 0.982
desv_pad_pop_tomados_foraDeCasa <- 1.183
n1<-170
n2<-170

# Critério: não se rejeita H0 se: Crítico < media1-media2 < Crítico
# 5% de significância
z5 <- 1.96

Critico_conhe <- z5*((desv_pad_pop_tomados_emCasa^2)/n1 + (desv_pad_pop_tomados_foraDeCasa)/n2)^0.5

# neste caso -0,3 não pertence ao intervalo [0,22;0,22] logo se rejeita H0 
# existem evidências estatísticas para afirmar que as médias não são iguais de
# gols sofridos dentro e fora de casa com um nível de significância de 5%


# PARA DESVIOS PADRÃO POPULACIONAIS DESCONHECIDOS MAS IGUAIS
# H0: média sofridos em casa = média sofridos fora de casa
# H1: média sofridos em casa DIFERENTE média sofridos fora de casa

var_tomados_emCasa_amostral <- var(df_HomeDortmund$FullTimeAwayGoals)
var_tomados_foraDeCasa_amostral <- var(df_AwayDortmund$FullTimeHomeGoals)

Sp_quadrado <- ((n1-1)*var_tomados_emCasa_amostral + (n2-1)*var_tomados_foraDeCasa_amostral)/(n1+n2-2)
Sp <- Sp_quadrado^0.5
# 336 graus de liberdade
t5 <- 1.9670
Critico_desc_igual <- t5*(Sp_quadrado)^0.5*(1/n1+1/n2)^0.5

# neste caso -0,3 não pertence ao intervalo [0,24;0,24] logo se rejeita H0 
# existem evidências estatísticas para afirmar que as médias não são iguais de
# gols sofridos dentro e fora de casa com um nível de significância de 5%



# PARA DESVIOS PADRÃO POPULACIONAIS DESCONHECIDOS E NÃO IGUAIS
# H0: média sofridos em casa = média sofridos fora de casa
# H1: média sofridos em casa DIFERENTE média sofridos fora de casa

w1 <- var_tomados_emCasa_amostral/n1
w2 <- var_tomados_foraDeCasa_amostral/n2

GL <- (w1+w2)^2/(((w1^2)/(n1+1))+((w2^2)/(n2+1))) - 2 
# Para um grau de liberdade de 339,0 tem-se t5 = 
t5GL339 <- -qt(0.025, 339.0039)


Critico_desc_nao_igual <- t5GL339*(var_tomados_emCasa_amostral/n1 + var_tomados_foraDeCasa_amostral/n2)^0.5

#tamanhos dos intervalos:

# desvios padrão conhecidos:
tamanho_intervalo_conhec <- 2*Critico_conhe

# desvios padrão desconhecidos e iguais:
tamanho_intervalo_desc_igual <- 2*Critico_desc_igual

# desvios padrão desconhecidos e diferentes:
tamanho_intervalo_desc_nao_igual <- 2*Critico_desc_nao_igual


# PARTE 2: 
# H0: variância sofridos em casa = variância sofridos fora de casa
# H1: variância sofridos em casa DIFERENTE variância sofridos fora de casa

# Para 5% de significância

F5GL169169 <- qf(0.025,169,169)^(-1)
Famostral <- var_tomados_foraDeCasa_amostral/var_tomados_emCasa_amostral















