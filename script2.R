###############################################################
#         Projeto de Métodos Estatísticos II - 1/2021         #
#                                                             #
#                                                             #
#               Análise Descritiva de Dados -                 #
#           Resultados do Saeb (ANEB/Prova Brasil) 2017       #
#                                                             #
#             Autor: Lucas Loureiro Lino da Costa             #                                                             #
#                                                             #
#                                                             #
# Script para análise dos dados                               #
# Data: 22/03/2021                                            #
###############################################################



###############################################################
# Instalação, carregamento dos pacotes necessários e configuração do
# diretório de trabalho e não utilização de notação científica

packages = c("ggplot2", "readr", "tidyverse",
             "reshape2", "ggforce", "devtools", "DescTools", "moments",
             "PropCIs")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

devtools::install_github(c("guiastrennec/ggplus", "r-spatial/classInt"))

library(ggplus)
library(ggplot2)
library(readr)
library(tidyverse)
library(reshape2)
library(DescTools)
library(classInt)
library(moments)
library(PropCIs)
setwd("/cloud/project")
options(scipen=999)


### DataSets e Ajustes ###

# Carregando os dados da pesquisa no sistema: amostra_190016566.csv
df_amostra = read_csv("DataSets/amostra.csv")

# Criação dos rótulos das Variáveis Categóricas de análise
df_amostra$REGIAO = factor(df_amostra$REGIAO,
                           labels = c("Norte", "Nordeste", "Sudeste",
                                      "Sul", "Centro-Oeste"),
                           levels = c(1, 2, 3, 4, 5))

df_amostra$AREA = factor(df_amostra$AREA,
                         labels = c("Capital", "Interior"), levels = c(1,2))

df_amostra$DEPENDENCIA_ADM = factor(df_amostra$DEPENDENCIA_ADM,
                                    labels = c("Federal", "Estadual",
                                               "Municipal"),
                                    levels = c(1:3))

df_amostra$SEXO = factor(df_amostra$SEXO,
                         labels = c("Masculino", "Feminino"),
                         levels = c("A", "B"))

df_amostra$RACA_COR = factor(df_amostra$RACA_COR,
                             labels = c("Branca", "Preta", "Parda",
                                        "Amarela","Indígena",
                                        "Não quero declarar"),
                             levels = c("A", "B", "C", "D", "E", "F"))

df_amostra$IDADE = factor(df_amostra$IDADE,
                          labels = c("8 anos ou menos",
                                     "9 anos", "10 anos",
                                     "11 anos", "12 anos","13 anos",
                                     "14 anos", "15 anos ou mais"),
                          levels = c("A", "B", "C", "D",
                                     "E", "F", "G", "H"))

df_amostra$MORA_MÃE = factor(df_amostra$MORA_MÃE,
                             labels = c("SIM", "Não",
                                        "Não, mas moro com outra mulher responsável por mim"),
                             levels = c("A", "B", "C"))

# Criação de um DataBase separado para as Variáveis Categóricas
# a serem analizadas
df_categoricas = df_amostra[, c(2, 5, 6, 10:12, 14 )]




###############################################################
#         Projeto de Métodos Estatísticos II - 1/2021         #
#                                                             #
#                                                             #
#               Análise Descritiva de Dados -                 #
#           Resultados do Saeb (ANEB/Prova Brasil) 2017       #
#                                                             #
#             Autor: Lucas Loureiro Lino da Costa             #                                                             #
#                                                             #
#                                                             #
# Script para análise dos dados                               #
# Data: 03/03/2021                                            #
###############################################################



###############################################################
# Instalação, carregamento dos pacotes necessários e configuração do
# diretório de trabalho e não utilização de notação científica

packages = c("ggplot2", "readr", "tidyverse",
             "reshape2", "ggforce", "devtools", "DescTools", "moments",
             "PropCIs")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

devtools::install_github(c("guiastrennec/ggplus", "r-spatial/classInt"))

library(ggplus)
library(ggplot2)
library(readr)
library(tidyverse)
library(reshape2)
library(DescTools)
library(classInt)
library(moments)
library(PropCIs)
setwd("/cloud/project")
options(scipen=999)


### DataSets e Ajustes ###

# Carregando os dados da pesquisa no sistema: amostra_190016566.csv
df_amostra = read_csv("DataSets/amostra.csv")

# Criação dos rótulos das Variáveis Categóricas de análise
df_amostra$REGIAO = factor(df_amostra$REGIAO,
                           labels = c("Norte", "Nordeste", "Sudeste",
                                      "Sul", "Centro-Oeste"),
                           levels = c(1, 2, 3, 4, 5))

df_amostra$AREA = factor(df_amostra$AREA,
                         labels = c("Capital", "Interior"), levels = c(1,2))

df_amostra$DEPENDENCIA_ADM = factor(df_amostra$DEPENDENCIA_ADM,
                                    labels = c("Federal", "Estadual",
                                               "Municipal"),
                                    levels = c(1:3))

df_amostra$SEXO = factor(df_amostra$SEXO,
                         labels = c("Masculino", "Feminino"),
                         levels = c("A", "B"))

df_amostra$RACA_COR = factor(df_amostra$RACA_COR,
                             labels = c("Branca", "Preta", "Parda",
                                        "Amarela","Indígena",
                                        "Não quero declarar"),
                             levels = c("A", "B", "C", "D", "E", "F"))

df_amostra$IDADE = factor(df_amostra$IDADE,
                          labels = c("8 anos ou menos",
                                     "9 anos", "10 anos",
                                     "11 anos", "12 anos","13 anos",
                                     "14 anos", "15 anos ou mais"),
                          levels = c("A", "B", "C", "D",
                                     "E", "F", "G", "H"))

df_amostra$MORA_MÃE = factor(df_amostra$MORA_MÃE,
                             labels = c("SIM", "Não",
                                        "Não, mas moro com outra mulher responsável por mim"),
                             levels = c("A", "B", "C"))

# Criação de um DataBase separado para as Variáveis Categóricas
# a serem analizadas
df_categoricas = df_amostra[, c(2, 5, 6, 10:12, 14 )]



### Criação das Amostras ###
# Criando as amostras de tamanho 30
amostras_30 = list()
for (i in 1:50) {
amostras_30[[i]] = df_amostra[sample(nrow(df_amostra), 30),]
}
# Criando as amostras de tamanho 50
amostras_100 = list()
for (i in 1:50) {
amostras_100[[i]] = df_amostra[sample(nrow(df_amostra), 100),]
}


###############################################################
# Variável AREA
# Tamanho 30
df_temp2 = df_categoricas[!is.na(df_categoricas$AREA),] %>%
  group_by(AREA) %>%
  select(AREA) %>%
  summarise(total = n(), percentage = n()/nrow(
  df_categoricas[!is.na(df_categoricas$AREA),]))

  
df_area = data.frame()
  
for (i in 1:50){
df_temp = amostras_30[[i]][!is.na(amostras_30[[i]]$AREA),] %>%
group_by(AREA) %>%
select(AREA) %>%
summarise(total = n(), percentage = n()/nrow(
amostras_30[[i]][!is.na(amostras_30[[i]]$AREA),]))
x = prop.test(df_temp$total[2], sum(df_temp$total), correct = FALSE)
df_area[i,1] = x$conf.int[1]
df_area[i,2] = x$conf.int[2]
df_area[i,3] = between(df_temp2$percentage[2], x$conf.int[1], x$conf.int[2])
}


# Gráfico dos Intervalos de confiança
# Variável Área - amostra de tamanho 30
ggplot(df_area, aes(x = 1:50, y = df_temp2$percentage[2])) +
geom_point()+
geom_errorbar(aes(ymax = V2, ymin = V1, colour = factor(V3))) +
coord_flip() +
labs(y ="Proporção de alunos que estudam em escolas do interior",
x = "Amostras de tamanho 30",
colour = "Média pertence\nao intervalo \nde confiança?")+
scale_colour_discrete(labels = c("Não", "Sim"))+
theme(panel.background = element_blank())


# Variável AREA
# Tamanho 100
df_temp2 = df_categoricas[!is.na(df_categoricas$AREA),] %>%
  group_by(AREA) %>%
  select(AREA) %>%
  summarise(total = n(), percentage = n()/nrow(
    df_categoricas[!is.na(df_categoricas$AREA),]))


df_area = data.frame()

for (i in 1:50){
  df_temp = amostras_100[[i]][!is.na(amostras_100[[i]]$AREA),] %>%
    group_by(AREA) %>%
    select(AREA) %>%
    summarise(total = n(), percentage = n()/nrow(
      amostras_100[[i]][!is.na(amostras_100[[i]]$AREA),]))
  x = prop.test(df_temp$total[2], sum(df_temp$total), correct = FALSE)
  df_area[i,1] = x$conf.int[1]
  df_area[i,2] = x$conf.int[2]
  df_area[i,3] = between(df_temp2$percentage[2], x$conf.int[1], x$conf.int[2])
}


# Gráfico dos Intervalos de confiança
# Variável Área - amostra de tamanho 100
ggplot(df_area, aes(x = 1:50, y = df_temp2$percentage[2])) +
  geom_point()+
  geom_errorbar(aes(ymax = V2, ymin = V1, colour = factor(V3))) +
  coord_flip() +
  labs(y ="Proporção de alunos que estudam em escolas do interior",
       x = "Amostras de tamanho 100",
       colour = "Média pertence\nao intervalo \nde confiança?")+
  scale_colour_discrete(labels = c("Não", "Sim"))+
  theme(panel.background = element_blank())


###############################################################
# Variável Sexo
# Tamanho 30
df_temp2 = df_categoricas[!is.na(df_categoricas$SEXO),] %>%
  group_by(SEXO) %>%
  select(SEXO) %>%
  summarise(total = n(), percentage = n()/nrow(
    df_categoricas[!is.na(df_categoricas$SEXO),]))


df_area = data.frame()

for (i in 1:50){
  df_temp = amostras_30[[i]][!is.na(amostras_30[[i]]$SEXO),] %>%
    group_by(SEXO) %>%
    select(SEXO) %>%
    summarise(total = n(), percentage = n()/nrow(
      amostras_30[[i]][!is.na(amostras_30[[i]]$SEXO),]))
  x = prop.test(df_temp$total[2], sum(df_temp$total), correct = FALSE)
  df_area[i,1] = x$conf.int[1]
  df_area[i,2] = x$conf.int[2]
  df_area[i,3] = between(df_temp2$percentage[2], x$conf.int[1], x$conf.int[2])
}


# Gráfico dos Intervalos de confiança
# Variável SEXO- amostra de tamanho 30
ggplot(df_area, aes(x = 1:50, y = df_temp2$percentage[2])) +
  geom_point()+
  geom_errorbar(aes(ymax = V2, ymin = V1, colour = factor(V3))) +
  coord_flip() +
  labs(y ="Proporção de alunas (sexo feminino)",
       x = "Amostras de tamanho 30",
       colour = "Média pertence\nao intervalo \nde confiança?")+
  scale_colour_discrete(labels = c("Não", "Sim"))+
  theme(panel.background = element_blank())


# Variável SEXO
# Tamanho 100
df_temp2 = df_categoricas[!is.na(df_categoricas$SEXO),] %>%
  group_by(SEXO) %>%
  select(SEXO) %>%
  summarise(total = n(), percentage = n()/nrow(
    df_categoricas[!is.na(df_categoricas$SEXO),]))


df_area = data.frame()

for (i in 1:50){
  df_temp = amostras_100[[i]][!is.na(amostras_100[[i]]$SEXO),] %>%
    group_by(SEXO) %>%
    select(SEXO) %>%
    summarise(total = n(), percentage = n()/nrow(
      amostras_100[[i]][!is.na(amostras_100[[i]]$SEXO),]))
  x = prop.test(df_temp$total[2], sum(df_temp$total), correct = FALSE)
  df_area[i,1] = x$conf.int[1]
  df_area[i,2] = x$conf.int[2]
  df_area[i,3] = between(df_temp2$percentage[2], x$conf.int[1], x$conf.int[2])
}


# Gráfico dos Intervalos de confiança
# Variável SEXO - amostra de tamanho 100
ggplot(df_area, aes(x = 1:50, y = df_temp2$percentage[2])) +
  geom_point()+
  geom_errorbar(aes(ymax = V2, ymin = V1, colour = factor(V3))) +
  coord_flip() +
  labs(y ="Proporção de alunas (sexo feminino)",
       x = "Amostras de tamanho 100",
       colour = "Média pertence\nao intervalo \nde confiança?")+
  scale_colour_discrete(labels = c("Não", "Sim"))+
  theme(panel.background = element_blank())


###############################################################
# Variável Nota_LP
# Tamanho 30
df_area = data.frame()

for (i in 1:50){
  x = t.test(amostras_30[[i]]$NOTA_LP)
  df_area[i,1] = x$conf.int[1]
  df_area[i,2] = x$conf.int[2]
  df_area[i,3] = between(mean(df_amostra$NOTA_LP), x$conf.int[1], x$conf.int[2])
}


# Gráfico dos Intervalos de confiança
# Variável Nota_LP- amostra de tamanho 30
ggplot(df_area, aes(x = 1:50, y = mean(df_amostra$NOTA_LP))) +
  geom_point()+
  geom_errorbar(aes(ymax = V2, ymin = V1, colour = factor(V3))) +
  coord_flip() +
  labs(y ="Média das notas de Língua Portuguesa",
       x = "Amostras de tamanho 30",
       colour = "Média pertence\nao intervalo \nde confiança?")+
  scale_colour_discrete(labels = c("Não", "Sim"))+
  theme(panel.background = element_blank())


# Variável Nota_LP
# Tamanho 100
df_area = data.frame()

for (i in 1:50){
  x = t.test(amostras_100[[i]]$NOTA_LP)
  df_area[i,1] = x$conf.int[1]
  df_area[i,2] = x$conf.int[2]
  df_area[i,3] = between(mean(df_amostra$NOTA_LP), x$conf.int[1], x$conf.int[2])
}


# Gráfico dos Intervalos de confiança
# Variável Nota_LP- amostra de tamanho 100
ggplot(df_area, aes(x = 1:50, y = mean(df_amostra$NOTA_LP))) +
  geom_point()+
  geom_errorbar(aes(ymax = V2, ymin = V1, colour = factor(V3))) +
  coord_flip() +
  labs(y ="Média das notas de Língua Portuguesa",
       x = "Amostras de tamanho 100",
       colour = "Média pertence\nao intervalo \nde confiança?")+
  scale_colour_discrete(labels = c("Não", "Sim"))+
  theme(panel.background = element_blank())


###############################################################################
# Gráfico dos Intervalos de confiança
# Variável Nota_MT - amostra de tamanho 30
df_area = data.frame()

for (i in 1:50){
  x = t.test(amostras_30[[i]]$NOTA_MT)
  df_area[i,1] = x$conf.int[1]
  df_area[i,2] = x$conf.int[2]
  df_area[i,3] = between(mean(df_amostra$NOTA_MT), x$conf.int[1], x$conf.int[2])
}


# Gráfico dos Intervalos de confiança
# Variável Nota_LP- amostra de tamanho 30
ggplot(df_area, aes(x = 1:50, y = mean(df_amostra$NOTA_MT))) +
  geom_point()+
  geom_errorbar(aes(ymax = V2, ymin = V1, colour = factor(V3))) +
  coord_flip() +
  labs(y ="Média das notas de Matemática",
       x = "Amostras de tamanho 30",
       colour = "Média pertence\nao intervalo \nde confiança?")+
  scale_colour_discrete(labels = c("Não", "Sim"))+
  theme(panel.background = element_blank())


# Variável Nota_MT - amostra de tamanho 100
df_area = data.frame()

for (i in 1:50){
  x = t.test(amostras_100[[i]]$NOTA_MT)
  df_area[i,1] = x$conf.int[1]
  df_area[i,2] = x$conf.int[2]
  df_area[i,3] = between(mean(df_amostra$NOTA_MT), x$conf.int[1], x$conf.int[2])
}


# Gráfico dos Intervalos de confiança
# Variável Nota_LP- amostra de tamanho 100
ggplot(df_area, aes(x = 1:50, y = mean(df_amostra$NOTA_MT), color = "green")) +
  geom_point()+
  geom_errorbar(aes(ymax = V2, ymin = V1, colour = factor(V3))) +
  coord_flip() +
  labs(y ="Média das notas de Matemática",
       x = "Amostras de tamanho 100",
       colour = "Média pertence\nao intervalo \nde confiança?")+
  scale_colour_discrete(labels = c("Não", "Sim"))+
  theme(panel.background = element_blank())
