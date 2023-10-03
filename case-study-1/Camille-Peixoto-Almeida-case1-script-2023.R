#################### Camille Peixoto Almeida 12702259 #######################

# importação de bibliotecas
library(tidyverse)
library(ggplot2)

# selecionar a base de dados
df <- readRDS("imdb.rds")

# retirar os espacos nulos (NA)
df <- na.omit(df)

# excluir a coluna pais
df$pais = NULL

# definir a coluna idade
df$idade = 2023 - df$ano

# definir a coluna velho ou novo
df$status = "Novo/Velho"

# Caracterizar velho/novo idades >= a 50 (velhos) e < 50 (novos)
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

# criar uma base de dados nova as novas modificacoes
write_rds(df,"banco_de_dados_case0_camille")

#criei um banco de dados 
df_base_de_dados_velho_novo <- readRDS("banco_de_dados_case0_camille")

###############################################################################
###############################################################################
#criar um banco de dados de velhos
df_velhos <- subset(df_base_de_dados_velho_novo, df_base_de_dados_velho_novo$status == "Velho")


###########################     duracao     #################################

media_duracao_dos_velhos <- mean(df_velhos$duracao)
mediana_duracao_dos_velhos <- median(df_velhos$duracao)

# analisou-se o banco de dados df_velhos por meio da função "table" no proprio 
# console, pois assim conseguimos contabilizar o número de vezes que cada valor
# aparece (a frequência)

table(df_velhos$duracao)
frequencia_duracao_velhos <- table(df_velhos$duracao)

# observou-se diretamente que dentre os 41 elementos "velhos" existem 2 modas (duracao = 108 e 152, frequência = 3)
moda_duracao_dos_velhos <- names(frequencia_duracao_velhos[frequencia_duracao_velhos == max(frequencia_duracao_velhos)])

variancia_duracao_dos_velhos <- var(df_velhos$duracao)

desvio_padrao_duracao_dos_velhos <- sd(df_velhos$duracao)
  
amplitude_duracao_dos_velhos <- max(df_velhos$duracao)-min(df_velhos$duracao)

###########################     orcamento    #################################
media_orcamento_dos_velhos <- mean(df_velhos$orcamento)

mediana_orcamento_dos_velhos <- median(df_velhos$orcamento)

# tabelo novamente os 41 elementos "velhos" para dados de orcamento:
table(df_velhos$orcamento)
frequencia_orcamento_dos_velhos <- table(df_velhos$orcamento)

# observou-se dentre os 41 elementos "velhos" existe 1 moda  
# (orcamento = 6000000,frequência 4)

moda_orcamento_dos_velhos <-  names(frequencia_orcamento_dos_velhos[frequencia_orcamento_dos_velhos == max(frequencia_orcamento_dos_velhos)])

variancia_orcamento_dos_velhos <- var(df_velhos$orcamento)

desvio_padrao_orcamento_dos_velhos <- sd(df_velhos$orcamento)

amplitude_orcamento_dos_velhos <- max(df_velhos$orcamento)-min(df_velhos$orcamento)

###########################     nota_imdb     #################################

media_nota_imdb_dos_velhos <- mean(df_velhos$nota_imdb)

mediana_nota_imdb_dos_velhos <- median(df_velhos$nota_imdb)

# tabelo novamente os 41 elementos "velhos" para dados da nota imdb:

table(df_velhos$nota_imdb)

frequencia_nota_imdb_dos_velhos <- table(df_velhos$nota_imdb)

# observou-se dentre os 41 elementos "velhos" existe 1 moda  
# (nota_imdb = 8,1 / frequência = 5)

moda_nota_imdb_dos_velhos <-  names(frequencia_nota_imdb_dos_velhos[frequencia_nota_imdb_dos_velhos == max(frequencia_nota_imdb_dos_velhos)])

variancia_nota_imdb_dos_velhos <- var(df_velhos$nota_imdb)

desvio_padrao_nota_imdb_dos_velhos <- sd(df_velhos$nota_imdb)

amplitude_nota_imdb_dos_velhos <- max(df_velhos$nota_imdb)-min(df_velhos$nota_imdb)

###############################################################################
###############################################################################
# criar um banco de dados de novos

df_novos <- subset(df_base_de_dados_velho_novo, df_base_de_dados_velho_novo$status == "Novo")

###########################     duracao     #################################

media_duracao_dos_novos <- mean(df_novos$duracao)

mediana_duracao_dos_novos <- median(df_novos$duracao)

# analisou-se o banco de dados df_novos por meio da função "table" no proprio 
# console, pois assim conseguimos contabilizar o número de vezes que cada valor
# aparece (a frequência)

table(df_novos$duracao)
frequencia_duracao_novos <- table(df_novos$duracao)

# observou-se dentre os 2946 elementos "novos" existe 1 moda (duracao = 101, frequência = 90)

moda_duracao_dos_novos <- names(frequencia_duracao_novos[frequencia_duracao_novos == max(frequencia_duracao_novos)])

variancia_duracao_dos_novos <- var(df_novos$duracao)

desvio_padrao_duracao_dos_novos <- sd(df_novos$duracao)

amplitude_duracao_dos_novos <- max(df_novos$duracao)-min(df_novos$duracao)


###########################     orcamento    #################################

media_orcamento_dos_novos <- mean(df_novos$orcamento)

mediana_orcamento_dos_novos <- median(df_novos$orcamento)

# tabelo novamente os 2946 elementos "novos" para dados de orcamento:
table(df_novos$orcamento)
frequencia_orcamento_dos_novos <- table(df_novos$orcamento)

# observou-se dentre os 2946 elementos "novos" existe 1 moda  
# (orcamento = 20000000 ,frequência = 127)

moda_orcamento_dos_novos <-  names(frequencia_orcamento_dos_novos[frequencia_orcamento_dos_novos == max(frequencia_orcamento_dos_novos)])

variancia_orcamento_dos_novos <- var(df_novos$orcamento)

desvio_padrao_orcamento_dos_novos <- sd(df_novos$orcamento)

amplitude_orcamento_dos_novos <- max(df_novos$orcamento)-min(df_novos$orcamento)


###########################     nota_imdb     #################################

media_nota_imdb_dos_novos <- mean(df_novos$nota_imdb)

mediana_nota_imdb_dos_novos <- median(df_novos$nota_imdb)

# tabelo novamente os 2946 elementos "novos" para dados da nota imdb:

table(df_novos$nota_imdb)

frequencia_nota_imdb_dos_novos <- table(df_novos$nota_imdb)

# observou-se dentre os 2946 elementos "novos" existe 1 moda  
# (nota_imdb = 6,7 / frequência = 139)

moda_nota_imdb_dos_novos <-  names(frequencia_nota_imdb_dos_novos[frequencia_nota_imdb_dos_novos == max(frequencia_nota_imdb_dos_novos)])

variancia_nota_imdb_dos_novos <- var(df_novos$nota_imdb)

desvio_padrao_nota_imdb_dos_novos <- sd(df_novos$nota_imdb)

amplitude_nota_imdb_dos_novos <- max(df_novos$nota_imdb)-min(df_novos$nota_imdb)

###############################################################################
###############################################################################
################    construção dos gráficos boxplot     #######################


############################### duracao ####################################

# grupos a serem comparados : duracao de df_velhos e duracao de df_novos
# criar um vetor com duracao de df_velhos e duracao de df_novos

vetor_duracao <- data.frame(grupo = c(rep("Velhos", length(df_velhos$duracao)), rep("Novos", length(df_novos$duracao))),
                              valores = c(df_velhos$duracao, df_novos$duracao))

# plotar dois boxplots usando ggplot2

ggplot(vetor_duracao, aes(x = grupo, y = valores)) +
  
  geom_errorbar(stat = "boxplot", width = 0.2)+ # barras 
  
  geom_boxplot(width = 0.6, fill = "grey90",outlier.shape = 1, outlier.size = 2) + # tamanho da caixa
  
  labs(title = "Comparação de duração entre elementos velhos e novos", x = "Elementos", y = "Duração (min)")+

theme_classic()

############################### orcamento ####################################

# grupos a serem comparados : orcamento de df_velhos e orcamento de df_novos
# criar um vetor com orcamento de df_velhos e orcamento de df_novos

vetor_orcamento <- data.frame(grupo = c(rep("Velhos", length(df_velhos$orcamento)), rep("Novos", length(df_novos$orcamento))),
                              valores = c(df_velhos$orcamento/10^6, df_novos$orcamento/10^6))

# plotar dois boxplots usando ggplot2

ggplot(vetor_orcamento, aes(x = grupo, y = valores)) +
  
  geom_errorbar(stat = "boxplot", width = 0.2)+ # barras 
  
  geom_boxplot(width = 0.6, fill = "grey90",outlier.shape = 1, outlier.size = 2) + # tamanho da caixa
  
  labs(title = "Comparação de orçamento entre elementos velhos e novos", x = "Elementos", y = "Orçamento em milhões de unidades de moeda")+

theme_classic()


# Boxplot único para orçamento Velhos 
ggplot(df_velhos, aes(y = df_velhos$orcamento/10^6, x = "")) +
  
  geom_errorbar(stat = "boxplot", width = 0.2)+ # barras 
  
  geom_boxplot(width = 0.6, 
               fill = "grey90",
               outlier.size = 2, 
               outlier.shape = 1) + # tamanho da caixa
  
  geom_point(stat="summary", fun= "mean", col = "blue", shape = 15)+
  
  labs(title = "Orçamento dos elementos velhos ",y = "Orçamento em milhões de unidades de moeda", x = "Elementos velhos")+

theme_classic()


# Boxplot único para orçamento Novos
ggplot(df_novos, aes(y = df_novos$orcamento/10^6, x = "")) +
  
  geom_errorbar(stat = "boxplot", width = 0.2)+ # barras 
  
  geom_boxplot(width = 0.6, fill = "grey90",outlier.size = 2, outlier.shape = 1) + # tamanho da caixa
  
  geom_point(stat="summary", fun= "mean", col = "blue", shape = 15)+
  
  labs(title = "Orçamento dos elementos novos ",y = "Orçamento em milhões de unidades de moeda", x = "Elementos novos")+

theme_classic()


############################### NOTA IMDB ####################################

# grupos a serem comparados : nota_imbd de df_velhos e nota_imdb de df_novos
# criar um vetor com nota_imbd de df_velhos e nota_imdb de df_novos

vetor_nota_imdb <- data.frame(grupo = c(rep("Velhos", length(df_velhos$nota_imdb)), rep("Novos", length(df_novos$nota_imdb))),
                    valores = c(df_velhos$nota_imdb, df_novos$nota_imdb))

# plotar dois boxplots usando ggplot2

ggplot(vetor_nota_imdb, aes(x = grupo, y = valores)) +
  
  geom_errorbar(stat = "boxplot", width = 0.2)+ # barras 
  
  geom_boxplot(width = 0.6, fill = "grey90",outlier.shape = 1, outlier.size = 2) + # tamanho da caixa
  
  geom_point(stat="summary", fun= "mean", col = "blue", shape = 15)+
  
  labs(title = "Comparação de nota IMDB entre elementos velhos e novos", x = "Elementos", y = "Nota IMDB (adimensional)")+
  
theme_classic()
  
#############################################################################
#############################################################################
#############################    histogramas   #############################

# histograma simples  - nota imdb
hist(df_novos$nota_imdb,
      breaks = 30,
      freq = T,
      col = "yellow",
      ylab = "Frequência",
      xlab = "Nota IMDB",
      main = "Histograma da Nota IMDB para elementos novos")  
  
# densidade de probabilidade
hist(df_novos$nota_imdb,
     breaks = 30, 
     freq = F,
     col = "yellow",
     ylab = "Frequência relativa das nota IMDB",
     xlab = "Nota IMDB",
     main = "Função densidade de probabilidade \n das notas IMDB dos elementos novos")
  densidade <- density(df_novos$nota_imdb)
  lines(densidade, lwd = 2)

# densidade de probabilidade com a linha vertical escrito média
hist(df_novos$nota_imdb,
       breaks = 30, 
       freq = F,
       col = "yellow",
       ylab = "Frequência relativa das nota IMDB",
       xlab = "Nota IMDB",
       main = "Função densidade de probabilidade \n das notas IMDB dos elementos novos")
  media <- mean(df_novos$nota_imdb)
  densidade <- density(df_novos$nota_imdb)
  lines(densidade, lwd = 2)
  abline(v =media, lwd = 5, col = "blue")
  text(x = media - 0.5, y = 0.41, "Média", col = "blue")

# histograma simples - duração - velhos
hist(df_velhos$duracao,
       breaks = 100,
       freq = F,
       col = "yellow",
       ylab = "Probabilidade",
       xlab = "Duração (minutos)",
       main = "Probabilidade \n da duração para elementos velhos")  

# histograma simples - duração - novos
hist(df_novos$duracao,
       breaks = 100,
       freq = F,
       col = "yellow",
       ylab = "Probabilidade",
       xlab = "Duração (minutos)",
       main = "Probabilidade \n da duração para elementos novos")  

# histograma simples - orçamento - velhos
hist(df_velhos$orcamento/10^6,
     breaks = 100,
     freq = F,
     col = "yellow",
     ylab = "Probabilidade",
     xlab = "Orçamento em milhões de unidades de moedas",
     main = "Probabilidade \n do orçamento para elementos velhos") 


# histograma simples - orçamento - novos
hist(df_novos$orcamento/10^6,
     breaks = 600,
     freq = F,
     col = "yellow",
     ylab = "Probabilidade",
     xlab = "Orçamento em milhões de unidades de moedas",
     main = "Probabilidade \n do orçamento para elementos novos")  

# histograma simples  - nota imdb
hist(df_velhos$nota_imdb,
     breaks = 100,
     freq = T,
     col = "yellow",
     ylab = "Frequência",
     xlab = "Nota IMDB",
     main = "Histograma da Nota IMDB para elementos velhos")  


d_media_mediana_velhos_orcamento <- (media_orcamento_dos_velhos - mediana_orcamento_dos_velhos)/media_orcamento_dos_velhos 
d_media_mediana_novos_orcamento <- (media_orcamento_dos_novos - mediana_orcamento_dos_novos)/media_orcamento_dos_novos

###############################################################################
###############################################################################
###############################################################################
############################## Amostragem #####################################


Z <- sample(1:2987,200,replace = TRUE)


amostra_velhos <- df[Z[1:200]]

