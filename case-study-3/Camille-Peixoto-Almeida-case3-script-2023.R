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


###############################  LETRA A ##################################
# Criar gráfico da evolução temporal dos preços de fechamento das criptomoedas
ggplot(df, aes(x = df$date, y = df$close)) +
  geom_point() +
  labs(title = "Evolução temporal do preço das criptomoedas\n (Dogecoin, Stellar e XRP)", 
       x = "Tempo (dias-anos)", 
       y = "Preço das criptomoedas\n  (unidade de moeda)") +
  theme_minimal()

# definir a coluna RETORNO
df$RETORNO = 1
for (i in 2:5333) {
  df$RETORNO[i] <- log(df$close[i]/df$close[i-1], base = exp(1))
}

#criar um banco de dados para apenas quando o retorno for maior que 2,5 %
retorno_0.25 <- subset(df,df$RETORNO >= 0.025)

#criei 3 banco de dados separados para cada moeda
df_Dogecoin <- subset(df, df$name == "Dogecoin")
df_Stellar <- subset(df,df$name == "Stellar")
df_XRP <- subset(df,df$name == "XRP")

# criei outra coluna chamada retorno para cada um dos dados para que o dado da 
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

retorno_doge_2.5 <- subset(df_Dogecoin,df_Dogecoin$retornoDogecoin >= 0.025)
retorno_ste_2.5 <- subset(df_Stellar,df_Stellar$retornoStellar >= 0.025)
retorno_XRP_2.5 <- subset(df_XRP,df_XRP$retornoXRP >= 0.025)

# Gráfico da evolução temporal do retorno das criptomoedas Dogecoin
ggplot(df_Dogecoin, aes(x = df_Dogecoin$date, y = df_Dogecoin$retornoDogecoin)) +
  geom_point() +
  labs(title = "Evolução temporal do retorno das criptomoedas Dogecoin", 
       x = "Tempo (dias-anos)", 
       y = "Retorno - (admensional)") +
  theme_minimal()

# Box plot Dogecoin retorno
ggplot(df_Dogecoin, aes(y =retornoDogecoin, x = "")) +
  geom_errorbar(stat = "boxplot", width = 0.2)+ # barras 
  geom_boxplot(width = 0.5, fill = "grey90",outlier.size = 2, outlier.shape = 1) + # tamanho da caixa
  geom_point(stat="summary", fun= "mean", col = "blue", shape = 15)+
  labs(y = "Retorno (admensional)",x = "Dogecoin")+
  theme_classic()

# Gráfico da evolução temporal do retorno das criptomoedas Stellar
ggplot(df_Stellar, aes(x = df_Stellar$date, y = df_Stellar$retornoStellar)) +
  geom_point() +
  labs(title = "Evolução temporal do retorno das criptomoedas Stellar", 
       x = "Tempo (dias-anos)", 
       y = "Retorno - (adimensional)") +
  theme_minimal()

# boxplot Stellar

ggplot(df_Stellar, aes(y = retornoStellar, x = "")) +
  geom_errorbar(stat = "boxplot", width = 0.2)+ # barras 
  geom_boxplot(width = 0.5, 
               fill = "grey90",
               outlier.size = 2, 
               outlier.shape = 1) + # tamanho da caixa
  geom_point(stat="summary", fun= "mean", col = "blue", shape = 15)+
  labs(y = "Retorno (admensional)", x = "Stellar")+
  theme_classic()


# Gráfico da evolução temporal do retorno das criptomoedas XRP
ggplot(df_XRP, aes(x = df_XRP$date, y = df_XRP$retornoXRP)) +
  geom_point() +
  labs(title = "Evolução temporal do retorno das criptomoedas XRP", 
       x = "Tempo (dias-anos)", 
       y = "Retorno - (admensional)") +
  theme_minimal()

# boxplot retorno XRP
ggplot(df_XRP, aes(y = retornoXRP, x = "")) +
  geom_errorbar(stat = "boxplot", width = 0.2)+ # barras 
  geom_boxplot(width = 0.5, 
               fill = "grey90",
               outlier.size = 2, 
               outlier.shape = 1) + # tamanho da caixa
  geom_point(stat="summary", fun= "mean", col = "blue", shape = 15)+
  labs(y = "Retorno (admensional)", x = "XRP")+
  theme_classic()

###############################  LETRA C ##################################
# histograma para retornos maiores que 2,5%
hist(retorno_doge_2.5$date,
     breaks = 100,
     freq = T,
     col = "yellow",
     ylab = "Frequência",
     xlab = "Retorno > 2,5%",
     main = "Proporção dos dias:\n retorno maior que 2,5% - Dogecoin")

hist(retorno_ste_2.5$date,
     breaks = 150,
     freq = T,
     col = "yellow",
     ylab = "Frequência",
     xlab = "Retorno > 2,5%",
     main = "Proporção dos dias:\n retorno maior que 2,5% - Stellar") 

hist(retorno_XRP_2.5$date,
     breaks = 150,
     freq = T,
     col = "yellow",
     ylab = "Frequência",
     xlab = "Retorno > 2,5%",
     main = "Proporção dos dias:\n retorno maior que 2,5% - XRP") 

###############################  LETRA D ##################################
# histograma simples - CLOSE - DOGECOIN
hist(df_Dogecoin$close,
     breaks = 150,
     freq = T,
     col = "yellow",
     ylab = "",
     xlab = "",
     main = "Dogecoin") 
# histograma simples - RETORNO - DOGECOIN
hist(df_Dogecoin$RETORNO,
     breaks = 150,
     freq = T,
     col = "yellow",
     ylab = "",
     xlab = "",
     main = "Dogecoin")

###############################################################################
# histograma simples - CLOSE - STELLAR
hist(df_Stellar$close,
     breaks = 200,
     freq = T,
     col = "yellow",
     ylab = "",
     xlab = "",
     main = "Stellar") 

# histograma simples - RETORNO - STELLAR
hist(df_Stellar$RETORNO,
     breaks = 200,
     freq = T,
     col = "yellow",
     ylab = "",
     xlab = "",
     main = "Stellar")

###############################################################################
# histograma simples - CLOSE - XRP
hist(df_XRP$close,
     breaks = 200,
     freq = T,
     col = "yellow",
     ylab = "",
     xlab = "",
     main = "XRP") 

# histograma simples - RETORNO - XRP
hist(df_Stellar$RETORNO,
     breaks = 200,
     freq = T,
     col = "yellow",
     ylab = "Frequência",
     xlab = "Retorno",
     main = "XRP")

############ média, mediana, moda, variância, desvio padrão
media_Dogecoin <- mean(df_Dogecoin$retornoDogecoin)
media_Stellar <- mean(df_Stellar$retornoStellar)
media_XRP <- mean(df_XRP$retornoXRP)

mediana_Dogecoin <- median(df_Dogecoin$retornoDogecoin)
mediana_Stellar <- median(df_Stellar$retornoStellar)
mediana_XRP <- median(df_XRP$retornoXRP)

var_Dogecoin <- var(df_Dogecoin$retornoDogecoin)
var_Stellar <- var(df_Stellar$retornoStellar)
var_XRP <-var(df_XRP$retornoXRP)

desv_pad_Dogecoin <- sd(df_Dogecoin$retornoDogecoin)
desv_pad_Stellar <- sd(df_Stellar$retornoStellar)
desv_pad_XRP <- sd(df_XRP$retornoXRP)

freq_Dogecoin <- table(df_Dogecoin$retornoDogecoin) 
moda_Dogecoin <- names(freq_Dogecoin[freq_Dogecoin == max(freq_Dogecoin)])

freq_Stellar <- table(df_Stellar$retornoStellar) 
moda_Stellar <- names(freq_Stellar[freq_Stellar == max(freq_Stellar)])

freq_XRP <- table(df_XRP$retornoXRP) 
moda_XRP <- names(freq_XRP[freq_XRP == max(freq_XRP)])

# PARTE 2 - INTERVALO DE CONFIÂNÇA

retorno_doge_3 <- subset(df_Dogecoin,df_Dogecoin$retornoDogecoin >= 0.03)
retorno_ste_3 <- subset(df_Stellar,df_Stellar$retornoStellar >= 0.03)
retorno_XRP_3 <- subset(df_XRP,df_XRP$retornoXRP >= 0.03)

p_doge <- (381)/(1811) 
z <- 2.575
extremoD <- p_doge +z*(p_doge*(1-p_doge)/1811)^(0.5)
extremoE <- p_doge -z*(p_doge*(1-p_doge)/1811)^(0.5)
 


retorno_doge_20ele <- c(df_Dogecoin$retornoDogecoin[1792],
                        df_Dogecoin$retornoDogecoin[1793],
                        df_Dogecoin$retornoDogecoin[1794],
                        df_Dogecoin$retornoDogecoin[1795],
                        df_Dogecoin$retornoDogecoin[1796],
                        df_Dogecoin$retornoDogecoin[1797],
                        df_Dogecoin$retornoDogecoin[1798],
                        df_Dogecoin$retornoDogecoin[1799],
                        df_Dogecoin$retornoDogecoin[1800],
                        df_Dogecoin$retornoDogecoin[1801],
                        df_Dogecoin$retornoDogecoin[1802],
                        df_Dogecoin$retornoDogecoin[1803],
                        df_Dogecoin$retornoDogecoin[1804],
                        df_Dogecoin$retornoDogecoin[1805],
                        df_Dogecoin$retornoDogecoin[1806],
                        df_Dogecoin$retornoDogecoin[1807],
                        df_Dogecoin$retornoDogecoin[1808],
                        df_Dogecoin$retornoDogecoin[1809],
                        df_Dogecoin$retornoDogecoin[1810],
                        df_Dogecoin$retornoDogecoin[1811])

retorno_ste_20ele <- c(df_Stellar$retornoStellar[1539],
                       df_Stellar$retornoStellar[1540],
                       df_Stellar$retornoStellar[1541],
                       df_Stellar$retornoStellar[1542],
                       df_Stellar$retornoStellar[1543],
                       df_Stellar$retornoStellar[1544],
                       df_Stellar$retornoStellar[1545],
                       df_Stellar$retornoStellar[1546],
                       df_Stellar$retornoStellar[1547],
                       df_Stellar$retornoStellar[1548],
                       df_Stellar$retornoStellar[1549],
                       df_Stellar$retornoStellar[1570],
                       df_Stellar$retornoStellar[1571],
                       df_Stellar$retornoStellar[1572],
                       df_Stellar$retornoStellar[1573],
                       df_Stellar$retornoStellar[1574],
                       df_Stellar$retornoStellar[1575],
                       df_Stellar$retornoStellar[1576],
                       df_Stellar$retornoStellar[1577],
                       df_Stellar$retornoStellar[1578])

retorno_XRP_20ele <- c(df_XRP$retornoXRP[1925],
                       df_XRP$retornoXRP[1926],
                       df_XRP$retornoXRP[1927],
                       df_XRP$retornoXRP[1928],
                       df_XRP$retornoXRP[1929],
                       df_XRP$retornoXRP[1930],
                       df_XRP$retornoXRP[1931],
                       df_XRP$retornoXRP[1932],
                       df_XRP$retornoXRP[1933],
                       df_XRP$retornoXRP[1934],
                       df_XRP$retornoXRP[1935],
                       df_XRP$retornoXRP[1936],
                       df_XRP$retornoXRP[1937],
                       df_XRP$retornoXRP[1938],
                       df_XRP$retornoXRP[1939],
                       df_XRP$retornoXRP[1940],
                       df_XRP$retornoXRP[1941],
                       df_XRP$retornoXRP[1942],
                       df_XRP$retornoXRP[1943],
                       df_XRP$retornoXRP[1944])
var_doge_20ele<- var(retorno_doge_20ele)
var_ste_20ele<- var(retorno_ste_20ele)
var_XRP_20ele<- var(retorno_XRP_20ele)

hist(retorno_doge_20ele,
     breaks = 10,
     freq = T,
     col = "yellow",
     ylab = "Frequência",
     xlab = "Retorno",
     main = "Dogecoin - Retorno dos 20 últimos dias")

hist(retorno_ste_20ele,
     breaks = 10,
     freq = T,
     col = "yellow",
     ylab = "Frequência",
     xlab = "Retorno",
     main = "Stellar - Retorno dos 20 últimos dias")


hist(retorno_XRP_20ele,
     breaks = 15,
     freq = T,
     col = "yellow",
     ylab = "Frequência",
     xlab = "Retorno",
     main = "XRP - Retorno dos 20 últimos dias")
 
n<- 20
a <- 8.907
b <- 32.852
var <- var_XRP_20ele
  
extD <- (n-1)*(var)/a
extE <- (n-1)*(var)/b
  























