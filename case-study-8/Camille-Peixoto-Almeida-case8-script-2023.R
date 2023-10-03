library(dplyr)

library("tidyverse")

df_acoes <- readRDS("H:/Meu Drive/USP/semestres_passados/1°Quadri2023/reof_estat/Estudo de Caso 8 - ANOVA-20230531/Case8/acoes.rds")

## TESTE DE MÉDIAS

help("t.test")
# parâmetros
# x e y: bases de dados
# mu: valor verdadeiro da média (ou da diferença e médias)
# paired: indica se os dados são pareados ou não
# var.equal: indica se as variâncias são admitidas iguais
# conf.level: nível de significância

t.test(df_acoes$ITUB3,df_acoes$PRIO3)

# o p-valor é a área à direita a partir do valor amostral na distribuição t-student e representa a probabilidade de errar ao rejeitar H0

# H0: médias iguais
# H1: médias diferentes

# p-valor = 12,94%
# para 5% de sigficância => NÃO REJEITO H0

## TESTE DE VARI NCIAS

# o teste F é utilizado em testes de variâncias e sua distribuição é a F-Snedecor, que não é simétrica

help("bartlett.test")
# parâmetros
# x: base de dados
# g: indica os elementos correspondentes a x, caso este não seja uma lista
# formula: uma fórmula de "lhs" por "rhs", em que "lhs" devolve os valores da base de dados e "rhs", os valores correspondentes

# H0: variâncias homogêneas (iguais)
# H1: variâncias não homogêneas (diferentes)

df_acoes2 <- df_acoes %>% select(ITUB3)
df_acoes2$ação = "ITUB3"
colnames(df_acoes2) [1]<-c("valor")

df_acoes3 <- df_acoes %>% select(PRIO3)
df_acoes3$ação = "PRIO3"
colnames(df_acoes3) [1]<-c("valor")

df_acoes4 <- df_acoes %>% select(LWSA3)
df_acoes4$ação = "LWSA3"
colnames(df_acoes4) [1]<-c("valor")

df_acoes_bartlett<-rbind(df_acoes2,df_acoes3,df_acoes4)

bartlett.test(df_acoes_bartlett$valor,df_acoes_bartlett$ação)

# K-squared = 686.95
# p-value < 2.2e-16
# para 5% de sigficância => REJEITO H0, AFIRMO H1

## ANOVA UM FATOR 

df_tratamento <- readRDS("H:/Meu Drive/USP/semestres_passados/1°Quadri2023/reof_estat/Estudo de Caso 8 - ANOVA-20230531/Case8/tratamento.rds")
df_tratamento <- na.omit(df_tratamento)
# meio

meio_gf <- subset(df_tratamento, df_tratamento$MEIO == "GF")
meio_gc <- subset(df_tratamento, df_tratamento$MEIO == "GC")
meio_lj <- subset(df_tratamento, df_tratamento$MEIO == "LJ")

meio_gf <- na.omit(meio_gf)
meio_gc <- na.omit(meio_gc)
meio_lj <- na.omit(meio_lj)

boxplot(meio_gf$CRESCIMENTO,
        meio_gc$CRESCIMENTO,
        meio_lj$CRESCIMENTO, 
        names=c("GF","GC","LJ"), 
        xlab="Meio de desenvolvimento", 
        ylab="Crescimento", 
        main="Boxplots de Crescimento em relação ao meio", 
        col= c("red","yellow","orange"))

mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

mediaGF <- mean(meio_gf$CRESCIMENTO) # média = 1.655039
medianaGF <- median(meio_gf$CRESCIMENTO) # mediana = 1.870829
desvioGF <- sd(meio_gf$CRESCIMENTO) # desvio = 0.6571736
modaGF <- mode(meio_gf$CRESCIMENTO) # moda = 2.345208

mediaGC <- mean(meio_gc$CRESCIMENTO) # média = 1.40085
medianaGC <-median(meio_gc$CRESCIMENTO) # mediana = 1.224745
desvioGC <- sd(meio_gc$CRESCIMENTO) # desvio = 0.3349757
modaGC <- mode(meio_gc$CRESCIMENTO) # moda = 1.224745

mediaLJ <- mean(meio_lj$CRESCIMENTO) # média = 0.808651
medianaLJ <-median(meio_lj$CRESCIMENTO) # mediana = 0.707107
desvioLJ <- sd(meio_lj$CRESCIMENTO) # desvio = 0.3298223
modaLJ <- mode(meio_lj$CRESCIMENTO) # moda = 0.707107



cor(df_tratamento$GF, df_tratamento$GC)
cor(df_tratamento$GF, df_tratamento$LJ)

help(aov)

resultado_anova <- aov(formula = CRESCIMENTO ~ MEIO,data = df_tratamento)

summary(resultado_anova)

help("TukeyHSD")

TukeyHSD(resultado_anova)

############################################################################
# Análise de variância - ANOVA - Tratamento


tratamento_1 <- subset(df_tratamento, df_tratamento$TRAT == 1)
tratamento_2 <- subset(df_tratamento, df_tratamento$TRAT == 2)
tratamento_3 <- subset(df_tratamento, df_tratamento$TRAT == 3)

boxplot(tratamento_1$CRESCIMENTO,tratamento_2$CRESCIMENTO,tratamento_3$CRESCIMENTO, names=c("1","2","3"), xlab="Tratamento", ylab="Crescimento", main="Boxplots de Crescimento em relação ao Tratamento", col=c("red","yellow","orange"))

media_trat_1 <- mean(tratamento_1$CRESCIMENTO) # média = 1.261598
mediana_trat_1 <- median(tratamento_1$CRESCIMENTO) # mediana = 1.224745
desvio_trat_1 <- sd(tratamento_1$CRESCIMENTO) # desvio = 0.6061808
moda__trat_1 <- mode(tratamento_1$CRESCIMENTO) # moda = 0.707107

media_trat_2 <- mean(tratamento_2$CRESCIMENTO) # média = 1.173958
mediana_trat_2 <- median(tratamento_2$CRESCIMENTO) # mediana = 1.224745
desvio_trat_2 <- sd(tratamento_2$CRESCIMENTO) # desvio = 0.5445513
moda_trat_2 <- mode(tratamento_2$CRESCIMENTO) # moda = 0.707107

media_trat_3 <- mean(tratamento_3$CRESCIMENTO) # média = 1.139064
mediana_trat_3 <- median(tratamento_3$CRESCIMENTO) # mediana = 0.707107
desvio_trat_3 <- sd(tratamento_3$CRESCIMENTO) # desvio = 0.5801029
moda_trat_3 <- mode(tratamento_3$CRESCIMENTO) # moda = 0.707107

cor(tratamento_1$CRESCIMENTO, tratamento_3$CRESCIMENTO)

tratamento_1_novo <- slice(tratamento_1, -c(34, 32))
cor(tratamento_1_novo$CRESCIMENTO, tratamento_2$CRESCIMENTO)

resultado_anova_tratamento_1fator <- aov(CRESCIMENTO ~ TRAT, df_tratamento)
summary(resultado_anova_tratamento_1fator)
TukeyHSD(resultado_anova_tratamento_1fator)



## ANOVA DOIS FATORES

# meio e tratamento

resultado_anova_tratamento_2fatores <- aov(CRESCIMENTO ~ MEIO*TRAT, df_tratamento)
summary(resultado_anova_tratamento_2fatores)
TukeyHSD(resultado_anova_tratamento_2fatores)



