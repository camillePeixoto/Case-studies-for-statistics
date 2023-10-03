library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library("writexl")



# Camille Peixoto Almeida 12702259

# PARTE 1) Regressão linear teórica
df_vendas <- readRDS("H:/Meu Drive/USP/semestres_passados/1°Quadri2023/reof_estat/Estudo de Caso 9 - Regressão Linear-20230614/Case9/vendas.rds")

plot(df_vendas$vendas, df_vendas$tv, ylab = "Investimento na TV", xlab = "Número de vendas")
# Os pontos indicam visualmente que quanto maior o investimento em TV maior é o número de vendas. 



# PARTE 2) Regressão linear prática
help("lm")

modelo1 <-lm(df_vendas$vendas~df_vendas$tv, data = df_vendas) 
# os coeficientes obtidos foram: coeficiente angular =  0.5546 e coeficiente linear = 69.7482

abline(modelo1, col = "blue") + text(x = 90, y = 180, col = "blue","y = 0,5546x + 69,7482 \n R² = 0.81218")
# Sim a tendência encontrada foi a mesma que se percebeu apenas visualizando o gráfico de dispersão (quanto maior o investimento em TV maior o número de vendas)


#  PARTE 3)   Regressão linear: Teste de Hipótese

# H0: b0 = 0 e b1 = 0 (não há relação entre as variáveis de estudo) - o modelo 
# NÃO é significativo para explicar a variável y

# H1: b0 diferente de 0 e b1 diferente de 0 - o modelo é significativo para 
# explicar a variável y

summary(modelo1)
# R² =0.8112
# p-valor < 2.2 10^(-16) -> Como p-valor é muito menor que 5% (nível de significância usado como padrão no R) deve-se rejeitar a hipótese nula (H0), ou seja, pode-se afirmar que existe de fato uma relação linear entre investimento de TV e número de vendas.

# Para fazer o histograma:
df_residuals <- modelo1[["residuals"]]
media_residuos <- mean(df_residuals) 
# média dos resíduos = -5.3037e-16 -> muito próximo de zero
# Os resíduos da ANOVA podem ser usados para avaliar a adequação do modelo estatístico aos dados. Se os resíduos forem aleatórios, com média zero e variância constante, isso sugere que o modelo está capturando adequadamente a variabilidade dos dados.
# conclusão: o modelo estatístico empregado é adequado para representar os dados



hist(df_residuals,
     breaks = 35,
     freq = T,
     col = "yellow",
     ylab = "Frequência",
     xlab = "Resíduos",
     main = "Histograma dos resíduos encontrados",
     text(x = 30, y = 20, "média"))
abline(v = media_residuos, lwd = 5, col = "blue")
text(x = media_residuos + 17, y = 23, "Média dos \n resíduos", col = "blue")


# Fiz a função densidade de probabilidade (abaixo) para mostrar a distribuição
hist(df_residuals,
     breaks = 50,
     freq = F,
     col = "yellow",
     ylab = "Probabilidade",
     xlab = "Resíduos",
     main = "Função densidade de probabilidade \n dos resíduos encontrados",
     text(x = 30, y = 20, "média"))
densidade <- density(df_residuals)
lines(densidade, col = "blue" , lwd = 2)
abline(v = media_residuos, lwd = 5, col = "blue")
text(x = media_residuos + 10, y = 23, "Média dos\n resíduos", col = "blue")


# copiei os dados dos resíduos e passei para uma planilha, na linha abaixo eu mandei o RStudio ler
df_residual <- read_excel("H:/Meu Drive/USP/semestres_passados/1°Quadri2023/reof_estat/Estudo de Caso 9 - Regressão Linear-20230614/Case9/df_residuals.xlsx")



# TESTE DE HIPÓTESE PARA O PAR METRO: COEFICIENTE DE CORRELAÇÃO (ρ)
# ρ é estimado por r

# coeficiente de correlação
help(cor)

cor(df_vendas$vendas, df_vendas$tv)

cor.test(df_vendas$vendas, df_vendas$tv)

plot(df_residual$numero, df_residual$Residuals)

# PARTE 4)  Regressão Linear: Intervalo de Confiança e ANOVA 
tv = data.frame(tv = sort(df_vendas$tv))

estimados_IC <- predict(modelo1, tv, interval = "confidence")
previstos_IP <- predict(modelo1, tv, interval = "prediction")

df_estimados_IC <- read_excel("H:/Meu Drive/USP/semestres_passados/1°Quadri2023/reof_estat/Estudo de Caso 9 - Regressão Linear-20230614/estimados_IC.xlsx")
df_previstos_IP <- read_excel("H:/Meu Drive/USP/semestres_passados/1°Quadri2023/reof_estat/Estudo de Caso 9 - Regressão Linear-20230614/previstos_IP.xlsx")

ggplot(df_estimados_IC, aes(x = vendas, y = fit)) +
  geom_point(size = 2)+
  labs(title ="Intervalos de confiança para o investimento em TV",
       x = "Número de vendas", 
       y = "Investimento TV")+
  geom_errorbar(aes(ymin = lwr, ymax = upr))


ggplot(df_previstos_IP, aes(x = vendas, y = fit)) +
  geom_point(size = 2)+
  labs(title ="Intervalos de predição para o investimento em TV",
       x = "Número de vendas", 
       y = "Investimento TV")+
  geom_errorbar(aes(ymin = lwr, ymax = upr))
  
# ANOVA
aov(modelo1)

