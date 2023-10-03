# Camille Peixoto Almeida 12702259 - CASE 0

# importar a biblioteca
library(tidyverse)

# selecionarr a base de dados
df <- readRDS("H:/Meu Drive/USP/semestres_passados/1°Quadri2023/reof_estat/Estudo de Caso 5 - Teste de Hipóteses I-20230510/Case5/bundesliga.rds")

df_HomeDortmund <- subset(df,df$HomeTeam == "Dortmund")
df_AwayDortmund <- subset(df, df$AwayTeam == "Dortmund")


#boxplot(df$FullTimeHomeGoals, df$FullTimeAwayGoals, names= c("Em casa", "Fora de casa"), main = "Número de gols")


# desempenho do Dortmund em casa
ggplot(df_HomeDortmund, aes(y = df_HomeDortmund$FullTimeHomeGoals, x = "")) +
  
  geom_errorbar(stat = "boxplot", width = 0.2)+ # barras 
  
  geom_boxplot(width = 0.6, 
               fill = "grey90",
               outlier.size = 2, 
               outlier.shape = 1) + # tamanho da caixa
  
  geom_point(stat="summary", fun= "mean", col = "blue", shape = 15)+
  labs(title = "",y = "Número total de gols em casa", x = "Em casa")+
  geom_text(x = 0.89, y=2.5, label = "Média", col = "blue")+
  theme_classic()

# desempenho do Dortmund fora de casa 
ggplot(df_AwayDortmund, aes(y = df_AwayDortmund$FullTimeHomeGoals, x = "")) +
  
  geom_errorbar(stat = "boxplot", width = 0.2)+ # barras 
  
  geom_boxplot(width = 0.6, 
               fill = "grey90",
               outlier.size = 2, 
               outlier.shape = 1) + # tamanho da caixa
  
  geom_point(stat="summary", fun= "mean", col = "blue", shape = 15)+
  labs(title = "",y = "Número total de gols fora de casa", x = "Fora de casa")+
  geom_text(x = 0.89, y = 1.4, label = "Média", col = "blue")+
  theme_classic()


medidas_estat_Home <- summary(df_HomeDortmund$FullTimeHomeGoals)
medidas_estat_Away <- summary(df_AwayDortmund$FullTimeAwayGoals)


#teste de hipótese em casa

# H0: media0_Home = 2.6
# H1: media0_Home < 2.6
# nível de significância: 5% -> z = 1,645
summary(df_HomeDortmund$FullTimeHomeGoals)

desv_Home <- 0.5 
media0_Home <- 2.6
z95 <- 1.645
z99 <- 2.325
z<- z99
n_Home <- 170
Xcrit_Home <- media0_Home - z*desv_Home/(n_Home^0.5)
Xcal<- 2.465

# Como X amostral < Xcrit -> rejeito H0


# desvio padrão desconhecido 
# se eu não conheco a variância eu tenho mais incerteza e o intervalo aumenta 
#a amplitude do intervalo aumenta
desv_Home_desc <- sd(df_HomeDortmund$FullTimeHomeGoals)
t <- 2.326
Xcrit_Home_desc <- media0_Home - t*desv_Home_desc/(n_Home^0.5)

# Como Xcrit_Home_desc > Xcalc : rejeito H0
















