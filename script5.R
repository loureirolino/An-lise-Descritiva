###############################################################
#         Projeto de Métodos Estatísticos II - 1/2021         #
#                                                             #
#                                                             #
#               Comparação entre várias populações -          #
#           Resultados do Saeb (ANEB/Prova Brasil) 2017       #
#                                                             #
#             Autor: Lucas Loureiro Lino da Costa             #                                                             #
#                                                             #
#                                                             #
# Script para análise dos dados                               #
# Data: 01/05/2021                                            #
###############################################################


###############################################################
# Instalação, carregamento dos pacotes necessários e configuração do
# diretório de trabalho e não utilização de notação científica

packages = c("ggplot2", "readr", "tidyverse",
             "reshape2", "ggforce", "devtools", "DescTools", "moments",
             "PropCIs", "nortest", "car", "ggpubr", "nortest", "rstatix")
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
library(nortest)
library(car)
library("ggpubr")
library(nortest)
library("rstatix")

setwd("/cloud/project")
options(scipen=999)


### DataSets e Ajustes ###

# Carregando os dados da pesquisa no sistema: amostra_190016566.csv
df_amostra = read_csv("DataSets/amostra.csv")

# Removendo as entradas vazias com relação a USO_TEMPO_telas e
# untado a primeira categoria com a última

df_amostra = df_amostra[!is.na(df_amostra$USO_TEMPO_TELAS), ]

for (i in 1: nrow(df_amostra) ){
  if (df_amostra$USO_TEMPO_TELAS[i] == "E"){
    df_amostra$USO_TEMPO_TELAS[i] = "A"
  }
}

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

df_amostra$LOCALIZACAO = factor(df_amostra$LOCALIZACAO, 
                                labels = c("Urbana", "Rural"),
                                levels = c(1, 2))

df_amostra$USO_TEMPO_TELAS = factor(df_amostra$USO_TEMPO_TELAS,
                                    labels = c("Não usa ou menos de 1 hora",
                                               "Entre 1 e 2 horas",
                                               "Mais de 2 horas, até 3 horas",
                                               "Mais de 3 horas"),
                                    levels =c(LETTERS[1:4]))


###########################################################
### Criação da Amostra de tamanho 500 ###
set.seed(10)
amostras_500 = df_amostra[sample(nrow(df_amostra), 500),]


###########################################################
### Teste de Normalidade - Separado por grupo ###
# NOTA_MT e REGIAO #

amostras_500 %>% group_by(REGIAO) %>% count()

# Shapiro-Wilk = Norte, Centro-Oeste #
tapply(amostras_500$NOTA_MT, amostras_500$REGIAO,
       shapiro.test) # se p-valor > 0.05 - normal

# Anderson-Darling = Nordeste, Sudeste, Sul #
tapply(amostras_500$NOTA_MT, amostras_500$REGIAO,
       ad.test) # se p-valor > 0.05 - normal



# NOTA_LP e USO_TEMPO_TELAS #

amostras_500 %>% group_by(USO_TEMPO_TELAS) %>% count()

# Anderson-Darling = todas menos Mais de 2 horas, até 3 horas #
tapply(amostras_500$NOTA_LP, amostras_500$USO_TEMPO_TELAS,
       ad.test) # se p-valor > 0.05 - normal

# Shapiro-Wilk = Mais de 2 horas, até 3 horas #
tapply(amostras_500$NOTA_LP, amostras_500$USO_TEMPO_TELAS,
       shapiro.test) # se p-valor > 0.05 - normal


### Teste de Normalidade - Não separado por grupo ###
# Anderson-Darling #
ad.test(amostras_500$NOTA_MT)
ad.test(amostras_500$NOTA_LP)


###########################################################
### Teste de homocedasticidade - Separado por grupo ###
# teste robusto (usando a mediana) #
LeveneTest(NOTA_MT ~ REGIAO, amostras_500)
LeveneTest(NOTA_LP ~ USO_TEMPO_TELAS, amostras_500)


###########################################################
### Identificação de Outliers ###
# NOTA_MT e REGIAO #
amostras_500 %>%
  group_by(REGIAO) %>%
  identify_outliers(NOTA_MT)

# NOTA_LP e USO_TEMPO_TELAS #
amostras_500 %>%
  group_by(USO_TEMPO_TELAS) %>%
  identify_outliers(NOTA_LP)


###########################################################
### Anova Uma Via ###
# NOTA_MT e REGIAO #
anova_mt = aov(NOTA_MT ~ REGIAO, amostras_500)
summary(anova_mt)

# NOTA_LP e USO_TEMPO_TELAS #
anova_lp = aov(NOTA_LP ~ USO_TEMPO_TELAS, amostras_500)
summary(anova_lp)


###########################################################
### Teste de Kruskal-Wallis ###
# MT e regiao#
kruskal.test(NOTA_MT ~ REGIAO, data = amostras_500)

# LP e telas#
kruskal.test(NOTA_LP ~ USO_TEMPO_TELAS, amostras_500)


###########################################################
### Teste de Post-Hoc ###
# Teste de Dunn com ajuste do valor de p #
# MT e regiao#
dunn_test(NOTA_MT ~ REGIAO, data = amostras_500,
          p.adjust.method = "bonferroni")

# LP e telas#
dunn_test(NOTA_LP ~ USO_TEMPO_TELAS, data = amostras_500,
          p.adjust.method = "bonferroni")


###########################################################
### Boxplot ###
# NOTA_MT e REGIAO #
ggplot(amostras_500, aes(x = REGIAO, y = NOTA_MT, group = REGIAO)) +
  geom_boxplot(aes(fill = REGIAO)) +
  scale_fill_discrete(name = "Região da escola \n do estudante") +
  labs(y = "Proficiência em matemática", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# NOTA_LP e USO_TEMPO_TELAS #
ggplot(amostras_500, aes(x = USO_TEMPO_TELAS, y = NOTA_LP,
                        group = USO_TEMPO_TELAS)) +
  geom_boxplot(aes(fill = USO_TEMPO_TELAS)) +
  scale_fill_discrete(name = "Tempo gasto assistindo à TV") +
  labs(y = "Proficiência em português", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


###########################################################
### Medidas de dispersão ###
# NOta_MT e REGIAO #
# Amostra 500 #
tapply(amostras_500$NOTA_MT, amostras_500$LOCALIZACAO,summary)
tapply(amostras_500$NOTA_MT, amostras_500$LOCALIZACAO,var)
tapply(amostras_500$NOTA_MT, amostras_500$LOCALIZACAO,sd)


### Medidas de dispersão ###
# NOta_LP e USO_TEMPO_TELAS #
# Amostra 500 #
tapply(amostras_500$NOTA_LP, amostras_500$USO_TEMPO_TELAS,summary)
tapply(amostras_500$NOTA_LP, amostras_500$USO_TEMPO_TELAS,var)
tapply(amostras_500$NOTA_LP, amostras_500$USO_TEMPO_TELAS,sd)
