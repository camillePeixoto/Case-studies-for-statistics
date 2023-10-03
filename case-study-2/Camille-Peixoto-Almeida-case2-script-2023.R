#################### Camille Peixoto Almeida 12702259 #######################

# importação de bibliotecas
library(tidyverse)
library(ggplot2)

# selecionar a base de dados
df <- readRDS("imdb.rds")

# retirar os espacos nulos (NA)
df <- na.omit(df)

## definir a coluna idade
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
write_rds(df,"banco_de_dados_case2_camille")

#criei um banco de dados 
df_base_de_dados_velho_novo <- readRDS("banco_de_dados_case2_camille")

###############################################################################
###############################################################################
#criar um banco de dados de velhos
df_velhos <- subset(df_base_de_dados_velho_novo, df_base_de_dados_velho_novo$status == "Velho")

# criar um banco de dados de novos

df_novos <- subset(df_base_de_dados_velho_novo, df_base_de_dados_velho_novo$status == "Novo")

############################## Amostragem #####################################

### CASE 2
i<-1
vetor_medias = c()
vetor_var = c()

num_sorteio = 2600

while (i<=num_sorteio) {
  Z <- sample(1:2987, 200,replace = TRUE) # sortear 200 elementos dos 2987
  
  amostra <- df[Z[1:200],] # novas amostras
  
  vetor_medias[i]<- mean(amostra$duracao)
  vetor_var[i]<- var(amostra$duracao)
  
  i <- i + 1
}
 
media_das_medias_duracao <- mean(vetor_medias)
media_das_variancias_duracao <- mean(vetor_var)

media_duracao_populacao <- mean(df$duracao)
var_duracao_populacao <- var(df$duracao)
####### média das médias amostrais ####################
# histograma simples
hist(vetor_medias,
     breaks = 100,
     freq = T,
     col = "yellow",
     ylab = "Frequência",
     xlab = "Duração (minutos)",
     main = "Histograma de médias amostrais") 

# densidade de probabilidade
hist(vetor_medias,
     breaks = 100, 
     freq = F,
     col = "yellow",
     ylab = "Frequência relativa da duração",
     xlab = "Duração (minutos)",
     main = "Fdp - médias amostrais")
densidade <- density(vetor_medias)
lines(densidade, lwd = 2)
abline(v = media_das_medias_duracao, lwd = 5, col = "blue")
text(x = media_das_medias_duracao -0.7, y = -0.005, "Média", col = "blue")
 

####### média das variâncias amostrais  ####################
 
# histograma simples
hist(vetor_var,breaks = 100,freq = T, col = "yellow", ylab = "Frequência",xlab = "Duração (minutos)",
     main = "Histograma de variâncias amostrais") 

# densidade de probabilidade
hist(vetor_var,breaks = 100, freq = F,col = "yellow",ylab = "Frequência relativa da duração", xlab = "Duração (minutos)",
     main = "Fdp - variâncias amostrais")
densidade <- density(vetor_var)
lines(densidade, lwd = 2)
abline(v = media_das_variancias_duracao, lwd = 5, col = "blue")
text(x = media_das_variancias_duracao -0.7, y = -0.005, "Média", col = "blue")


media_duracao_dos_velhos <- mean(df_velhos$duracao)
media_duracao_dos_novos <- mean(df_novos$duracao)

variancia_duracao_dos_velhos <- var(df_velhos$duracao)
variancia_duracao_dos_novos <- var(df_novos$duracao)
# desvios padrão conhecidos
sigma_novos <- 25.2
sigma_velhos <- 27.12


# nivel de confiança de 90,95 ou 99 % 
#z1<- 2.575
#extremo_e_novos <- media_duracao_dos_novos - z1*sigma_novos/(2946)^(0.5)
#extremo_d_novos <- media_duracao_dos_novos + z1*sigma_novos/(2946)^(0.5)
  
#extremo_e_velhos <- media_duracao_dos_velhos - z1*sigma_velhos/(41)^(0.5)
#extremo_d_velhos <- media_duracao_dos_velhos + z1*sigma_velhos/(41)^(0.5)

# nivel de confiança de 90,95 ou 99 % 
t1 <- 2.576
extremo_e_novos_desc <- media_duracao_dos_novos - t1*(variancia_duracao_dos_novos/2946)^(0.5)
extremo_d_novos_desc <- media_duracao_dos_novos + t1*(variancia_duracao_dos_novos/2946)^(0.5)

t2<- 2.704
extremo_e_velhos_desc <- media_duracao_dos_velhos - t2*(variancia_duracao_dos_velhos/41)^(0.5)
extremo_d_velhos_desc <- media_duracao_dos_velhos + t2*(variancia_duracao_dos_velhos/41)^(0.5)

