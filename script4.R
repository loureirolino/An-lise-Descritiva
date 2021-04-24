###############################################################
#         Projeto de Métodos Estatísticos II - 1/2021         #
#                                                             #
#                                                             #
#               Comparação entre duas populações -            #
#           Resultados do Saeb (ANEB/Prova Brasil) 2017       #
#                                                             #
#             Autor: Lucas Loureiro Lino da Costa             #                                                             #
#                                                             #
#                                                             #
# Script para análise dos dados                               #
# Data: 23/04/2021                                            #
###############################################################


###############################################################
# Instalação, carregamento dos pacotes necessários e configuração do
# diretório de trabalho e não utilização de notação científica

packages = c("ggplot2", "readr", "tidyverse",
             "reshape2", "ggforce", "devtools", "DescTools", "moments",
             "PropCIs", "nortest", "car", "ggpubr")
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

df_amostra$LOCALIZACAO = factor(df_amostra$LOCALIZACAO, 
                                labels = c("Urbana", "Rural"),
                                levels = c(1, 2))


###########################################################
### Criação das Amostras ###
# Criando a amostra de tamanho 30
amostras_30 = df_amostra[sample(nrow(df_amostra), 30),]

# Criando aa amostra de tamanho 100
amostras_100 = df_amostra[sample(nrow(df_amostra), 100),]


###########################################################
### Teste de Normalidade - Separado por grupo ###
# NOTA_MT e LOCALIZAÇÃO #
# Amostra 30 #
tapply(amostras_30$NOTA_MT, amostras_30$LOCALIZACAO,
       shapiro.test) # como p-valor > 0.05 - normal

# NOTA_MT e LOCALIZAÇÃO #
# Amostra 100 #
tapply(amostras_100$NOTA_MT, amostras_100$LOCALIZACAO,
       shapiro.test) # como p-valor > 0.05 - normal

# NOTA_LP e DEPENDENCIA_ADM #
# Amostra 30 #
tapply(amostras_30$NOTA_LP, amostras_30$DEPENDENCIA_ADM,
       shapiro.test) # como p-valor > 0.05 - normal

# NOTA_LP e DEPENDENCIA_ADM #
# Amostra 100 #
tapply(amostras_100$NOTA_LP, amostras_100$DEPENDENCIA_ADM,
       shapiro.test) # como p-valor > 0.05 - normal

###########################################################
### Teste de homocedasticidade - Separado por grupo ###
# Amostra 30 #
LeveneTest(NOTA_LP ~ DEPENDENCIA_ADM, amostras_30)
LeveneTest(NOTA_MT ~ LOCALIZACAO, amostras_30)

# Amostra 100 #
LeveneTest(NOTA_LP ~ DEPENDENCIA_ADM, amostras_100)
LeveneTest(NOTA_MT ~ LOCALIZACAO, amostras_100)

###########################################################
### Teste-t para Amostras Independentes - bicaudal ###
# Amostra 30 #
t.test(NOTA_LP ~ DEPENDENCIA_ADM, amostras_30, var.equal = TRUE)
t.test(NOTA_MT ~ LOCALIZACAO, amostras_30, var.equal = TRUE)

# Amostra 100 #
t.test(NOTA_LP ~ DEPENDENCIA_ADM, amostras_100, var.equal = TRUE)
t.test(NOTA_MT ~ LOCALIZACAO, amostras_100, var.equal = TRUE)

###########################################################
### Boxplot ###
# NOTA_MT e LOCALIZAÇÃO #
# Amostra 30 #
ggplot(amostras_30, aes(x = LOCALIZACAO, y = NOTA_MT, group = LOCALIZACAO)) +
  geom_boxplot(aes(fill = LOCALIZACAO)) +
  scale_fill_discrete(name = "Localização da escola \n do estudante") +
  labs(y = "Proficiência em matemática", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# NOTA_MT e LOCALIZAÇÃO #
# Amostra 100 #
ggplot(amostras_100, aes(x = LOCALIZACAO, y = NOTA_MT, group = LOCALIZACAO)) +
  geom_boxplot(aes(fill = LOCALIZACAO)) +
  scale_fill_discrete(name = "Localização da escola \n do estudante") +
  labs(y = "Proficiência em matemática", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# NOTA_LP e DEPENDENCIA_ADM #
# Amostra 30 #
ggplot(amostras_30, aes(x = DEPENDENCIA_ADM, y = NOTA_LP,
                        group = DEPENDENCIA_ADM)) +
  geom_boxplot(aes(fill = DEPENDENCIA_ADM)) +
  scale_fill_discrete(name = "Esfera da escola \n do estudante") +
  labs(y = "Proficiência em português", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# NOTA_LP e DEPENDENCIA_ADM #
# Amostra 100 #
ggplot(amostras_100, aes(x = DEPENDENCIA_ADM, y = NOTA_LP,
                        group = DEPENDENCIA_ADM)) +
  geom_boxplot(aes(fill = DEPENDENCIA_ADM)) +
  scale_fill_discrete(name = "Esfera da escola \n do estudante") +
  labs(y = "Proficiência em português", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



###########################################################
### Medidas de dispersão ###
# NOta_MT e LOCALIZAÇÂO #
# Amostra 30 #
tapply(amostras_30$NOTA_MT, amostras_30$LOCALIZACAO,summary)
tapply(amostras_30$NOTA_MT, amostras_30$LOCALIZACAO,var)
tapply(amostras_30$NOTA_MT, amostras_30$LOCALIZACAO,sd)


### Medidas de dispersão ###
# NOta_MT e LOCALIZAÇÂO #
# Amostra 100 #
tapply(amostras_100$NOTA_MT, amostras_100$LOCALIZACAO,summary)
tapply(amostras_100$NOTA_MT, amostras_100$LOCALIZACAO,var)
tapply(amostras_100$NOTA_MT, amostras_100$LOCALIZACAO,sd)


### Medidas de dispersão ###
# NOta_LP e DEPENDENCIA_ADM #
# Amostra 30 #
tapply(amostras_30$NOTA_LP, amostras_30$DEPENDENCIA_ADM,summary)
tapply(amostras_30$NOTA_LP, amostras_30$DEPENDENCIA_ADM,var)
tapply(amostras_30$NOTA_LP, amostras_30$DEPENDENCIA_ADM,sd)


### Medidas de dispersão ###
# NOta_LP e DEPENDENCIA_ADM #
# Amostra 100 #
tapply(amostras_100$NOTA_LP, amostras_100$DEPENDENCIA_ADM,summary)
tapply(amostras_100$NOTA_LP, amostras_100$DEPENDENCIA_ADM,var)
tapply(amostras_100$NOTA_LP, amostras_100$DEPENDENCIA_ADM,sd)


###########################################################
### Teste de Normalidade ###
# NOTA_MT #
# Amostra 30 #
shapiro.test(amostras_30$NOTA_MT)

### Teste de Normalidade ###
# NOTA_LP #
# Amostra 30 #
shapiro.test(amostras_30$NOTA_LP)

###########################################################
### teste de postos com sinais de Wilcoxon - bilateral ###
# NOTA_LP  e NOTA_MT #
wilcox.test(amostras_30$NOTA_LP,amostras_30$NOTA_MT,
            paired=TRUE, alternative = c("two.sided"))
