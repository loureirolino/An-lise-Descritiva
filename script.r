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
             "reshape2", "ggforce", "devtools", "DescTools", "moments")
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
setwd("/cloud/project")
options(scipen=999)


### DataSets e Ajustes ###

# Carregando os dados da pesquisa no sistema: amostra_190016566.csv
df_amostra = read_csv("DataSets/amostra.csv")

# Criação dos rótulos das Variáveis Categóricas
df_amostra$REGIAO = factor(df_amostra$REGIAO,
                               labels = c("Norte", "Nordeste", "Sudeste",
                                          "Sul", "Centro-Oeste"),
                               levels = c(1, 2, 3, 4, 5))

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
df_categoricas = df_amostra[, c(2, 6, 10:12, 14 )]

###############################################################
###                       Análise                           ###
###                 Variáveis Categóricas                   ###
###############################################################

###############################################################
# Variável - REGIAO #   
# Frequências
df_temp = df_categoricas %>%
  group_by(REGIAO) %>%
  select(REGIAO) %>%
  summarise(total = n(), percentage = n()/dim(df_categoricas)[1]) %>%
  arrange(-total)

# Variável - REGIAO # 
# Histograma - Frequência Absoluta
df_temp %>%
  ggplot(aes(x = reorder(REGIAO, -total), fill = REGIAO,
             y = total)) +
  geom_histogram(stat = "identity") +
  scale_fill_discrete(name = "Região", breaks = df_temp$REGIAO) +
  labs(y = "Porcentagem", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Variável - REGIAO # 
# Histograma - Frequência Relativa
df_temp %>%
  ggplot(aes(x = reorder(REGIAO, -percentage), fill = REGIAO,
             y = percentage)) +
  geom_histogram(stat = "identity") +
  scale_y_continuous(labels = scales::percent)  +
  scale_fill_discrete(name = "Região", breaks = df_temp$REGIAO) +
  labs(y = "Porcentagem", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


###############################################################
# Variável - DEPENDENCIA_ADM #   
# Frequências
df_temp = df_categoricas %>%
  group_by(DEPENDENCIA_ADM) %>%
  select(DEPENDENCIA_ADM) %>%
  summarise(total = n(), percentage = n()/dim(df_categoricas)[1]) %>%
  arrange(-total)

# Variável - DEPENDENCIA_ADM #
# Pie-Chart - Frequência Absoluta
ggplot(df_temp) +
  geom_bar(aes(x= "", y = total, fill = DEPENDENCIA_ADM),
           stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(x=1, y = cumsum(total) - total/2, label = total)) +
  scale_fill_discrete(name = "Esfera") +
  theme_void()

# Variável - DEPENDENCIA_ADM #
# Pie-chart - Frequência Relativa
df_temp$label = scales::percent(df_temp$percentage)
ggplot(df_temp) +
  geom_bar(aes(x= "", y = percentage, fill = DEPENDENCIA_ADM),
           stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(x=1, y = cumsum(percentage) - percentage/2, label = label)) +
  scale_fill_discrete(name = "Esfera") +
  theme_void()

###############################################################
# Variável - SEXO #   
# Frequências
df_temp = df_categoricas %>%
  group_by(SEXO) %>%
  select(SEXO) %>%
  summarise(total = n(), percentage = n()/dim(df_categoricas)[1]) %>%
  arrange(-total)

# Variável - SEXO #
# Pie-chart - Frequência Absoluta
ggplot(df_temp) +
  geom_bar(aes(x= "", y = percentage, fill = SEXO),
           stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(x=1, y = cumsum(percentage) - percentage/2, label = total)) +
  scale_fill_discrete(name = "Sexo") +
  theme_void()

# Variável - SEXO #
# Pie-chart - Frequência Relativa
df_temp$label = scales::percent(df_temp$percentage)
ggplot(df_temp) +
  geom_bar(aes(x= "", y = percentage, fill = SEXO),
           stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(x=1, y = cumsum(percentage) - percentage/2, label = label)) +
  scale_fill_discrete(name = "Sexo") +
  theme_void()


###############################################################
# Variável - RACA_COR #   
# Frequências
# Removendo as entrads em branco do DB
df_temp = df_categoricas[!is.na(df_categoricas$RACA_COR),] %>%
  group_by(RACA_COR) %>%
  select(RACA_COR) %>%
  summarise(total = n(), percentage = n()/nrow(
    df_categoricas[!is.na(df_categoricas$RACA_COR),])) %>%
  arrange(-total)


# Variável - RACA_COR #
# Histograma - Frequência Absoluta
df_temp %>%
  ggplot(aes(x = reorder(RACA_COR, -total), fill = RACA_COR, y = total)) +
  geom_histogram(stat = "identity") +
  scale_fill_discrete(name = "Raça/Cor", breaks = df_temp$RACA_COR) +
  labs(y = "Frequência Absoluta", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Variável - RACA_COR #
# Histograma - Frequência Relativa
df_temp %>%
  ggplot(aes(x = reorder(RACA_COR, -percentage), fill = RACA_COR,
             y = percentage)) +
  geom_histogram(stat = "identity") +
  scale_y_continuous(labels = scales::percent)  +
  scale_fill_discrete(name = "Raça/Cor", breaks = df_temp$RACA_COR) +
  labs(y = "Porcentagem", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



###############################################################
# Variável - MORA_MAE #
# Frequências
# Removendo as entrads em branco do DB
df_temp = df_categoricas[!is.na(df_categoricas$MORA_MÃE),] %>%
  group_by(MORA_MÃE) %>%
  select(MORA_MÃE) %>%
  summarise(total = n(), percentage = n()/nrow(
    df_categoricas[!is.na(df_categoricas$MORA_MÃE),])) %>%
  arrange(-total)


# Variável - MORA_MAE #
# Histograma - Frequência Absoluta
# Removendo as entrads em branco do DB
df_categoricas[!is.na(df_categoricas$MORA_MÃE),] %>%
  ggplot(aes(x = MORA_MÃE, fill = MORA_MÃE)) +
  geom_histogram( stat = "count",bins = 6, na.rm = TRUE) +
  scale_fill_discrete(name = "Mora com a Mãe", labels = c("Sim", "Não",
                "Não, mas moro com outra\nmulher responsável por mim")) +
  labs(y = "Frequência Absoluta",
       x = "") +
  theme(panel.background = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
    
    
# Variável - MORA_MAE #
# Histograma - Frequência Relativa
# Removendo as entrads em branco do DB
df_categoricas[!is.na(df_categoricas$MORA_MÃE),] %>%
  ggplot(aes(x = MORA_MÃE, fill = MORA_MÃE)) +
  geom_histogram( stat = "count", aes(y = stat(count) / sum(count)),
                  bins = 6, na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Mora com a Mãe", labels = c("Sim", "Não",
              "Não, mas moro com outra\nmulher responsável por mim")) +
  labs(y = "Porcentagem",
       x = "") +
  theme(panel.background = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  

###############################################################


###############################################################
###                       Análise                           ###
###               Variáveis Quantitativas                   ###
###############################################################


###############################################################
# Variável - IDADE #
# Frequências
df_categoricas %>%
  group_by(IDADE) %>%
  select(IDADE) %>%
  summarise(total = n(), percentage = n()/dim(df_categoricas)[1])

# Variável - IDADE #
# Histograma - Frequência Absoluta
# Removendo as entrads em branco do DB
df_categoricas[!is.na(df_categoricas$IDADE),] %>%
  ggplot(aes(x = IDADE, fill = IDADE)) +
  geom_histogram( stat = "count") +
  scale_fill_discrete(name = "Idade do Estudante") +
  labs(y = "Frequência Absoluta",
       x = "") +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1))


# Variável - IDADE #
# Histograma - Frequência Relariva
# Removendo as entrads em branco do DB
df_categoricas[!is.na(df_categoricas$IDADE),] %>% 
  ggplot(aes(x = IDADE, fill = IDADE)) +
  geom_histogram( stat = "count", aes(y = stat(count) / sum(count)),
                  bins = 6, na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Idade do Estudante") +
  labs(y = "Porcentagem",
       x = "") +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1))


###############################################################
# Variável - NOTA_LP #
# Arredondando os valore das notas para duas casas decimais
df_amostra$NOTA_LP = round(df_amostra$NOTA_LP, 2)


# Variável - NOTA_LP #
# Extremos de NOTA_LP
max(df_amostra$NOTA_LP) #334.23
min(df_amostra$NOTA_LP) #97.68


# Variável - NOTA_LP #
# Criação de classes intervalares para Nota_LP (13)
# Amplitude de 20 unidades por intervalo
df_amostra_intervalar = df_amostra
array_nota_lp = classIntervals(df_amostra_intervalar$NOTA_LP,
                                       n= 13, style = "fixed",
                                       fixedBreaks = c(0,seq(100, 350, 20)))
df_amostra_intervalar$NOTA_LP = cut(df_amostra_intervalar$NOTA_LP,
                                    breaks = array_nota_lp$brks,
                                    labels = c("Menor que 100",
                                               "100 a 120",
                                               "120 a 140",
                                               "140 a 160",
                                               "160 a 180",
                                               "180 a 200",
                                               "200 a 220",
                                               "220 a 240",
                                               "240 a 260",
                                               "260 a 280",
                                               "280 a 300",
                                               "300 a 320",
                                               "320 a 340"))


# Variável - NOTA_LP #
# Frequências das classes intervalares
df_amostra_intervalar %>%
  group_by(NOTA_LP) %>%
  summarise(total = n(), percentage = round(n()/dim(df_categoricas)[1],2))


# Variável - NOTA_LP #
# Histograma - Distribuição
df_amostra_intervalar %>%
  ggplot(aes(x = NOTA_LP, fill = NOTA_LP)) +
  geom_histogram(binwidth = 5, stat = "count") +
  scale_fill_discrete(name = "Notas em\nLíngua Portuguesa") +
  labs(y = "Frequência",
       x = "") +
  theme(panel.background = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# Variável - NOTA_LP #
# Rampos e folhas  
stem(df_amostra$NOTA_LP)


# Variável - NOTA_LP #
# Medidas de posição e dispersão
summary(df_amostra$NOTA_LP)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 97.68  179.09  214.34  213.86  248.00  334.23
var(df_amostra$NOTA_LP)
# 2288.321
sd(df_amostra$NOTA_LP)
# 47.8364


# Variável - NOTA_LP #
# Medidas do Coeficiente de Curtose de Pearson e Coeficiente de Assimetria
round(kurtosis(df_amostra$NOTA_LP), 2)
# 2.53 - platicúrtica
round(skewness(df_amostra$NOTA_LP), 2)
# 0.07 - cauda do lado direito é relativamente maior que a do lado esquerdo


# Variável - NOTA_LP #
# Boxplot
df_amostra %>%
ggplot(aes(x="", y = NOTA_LP)) +
  geom_boxplot(outlier.colour = "red", outlier.shape=8, outlier.size=4,
               fill="gray", color="black") +
  stat_summary(fun = mean, geom = "point", shape=23, size = 4) +
  labs(x ="Português", y = "Notas")+
  theme(panel.background = element_blank())




###############################################################
# Variável - NOTA_MT

# Variável - NOTA_MT #
# Arredondando os valore das notas para duas casas decimais
df_amostra$NOTA_MT = round(df_amostra$NOTA_MT, 2)


# Variável - NOTA_MT #
# Extremos de NOTA_MT
max(df_amostra$NOTA_MT) #355.09
min(df_amostra$NOTA_MT) #111.87


# Variável - NOTA_MT #
# Criação de classes intervalares para Nota_MT (13)
# Amplitude de 20 unidades por intervalo
array_nota_MT = classIntervals(df_amostra_intervalar$NOTA_MT,
                               n= 13, style = "fixed",
                               fixedBreaks = c(seq(100, 360, 20)))
df_amostra_intervalar$NOTA_MT = cut(df_amostra_intervalar$NOTA_MT,
                                    breaks = array_nota_MT$brks,
                                    labels = c("100 a 120",
                                               "120 a 140",
                                               "140 a 160",
                                               "160 a 180",
                                               "180 a 200",
                                               "200 a 220",
                                               "220 a 240",
                                               "240 a 260",
                                               "260 a 280",
                                               "280 a 300",
                                               "300 a 320",
                                               "320 a 340",
                                               "340 a 360"))


# Variável - NOTA_MT #
# Frequências das classes intervalares
df_amostra_intervalar %>%
  group_by(NOTA_MT) %>%
  summarise(total = n(), percentage = round(n()/dim(df_categoricas)[1],2))


# Variável - NOTA_MT #
# Histograma - Distribuição
df_amostra_intervalar %>%
  ggplot(aes(x = NOTA_MT, fill = NOTA_MT)) +
  geom_histogram(binwidth = 5, stat = "count") +
  scale_fill_discrete(name = "Notas em\nMatemática") +
  labs(y = "Frequência", x = "") +
  theme(panel.background = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank())


# Variável - NOTA_MT #
# Rampos e folhas  
stem(df_amostra$NOTA_MT)


# Variável - NOTA_MT #
# Medidas de posição e dispersão
summary(df_amostra$NOTA_MT)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 111.9   189.3   222.7   223.8   256.3   355.1
var(df_amostra$NOTA_MT)
# 2228.236
sd(df_amostra$NOTA_MT)
#47.20419


# Variável - NOTA_MT #
# Medidas do Coeficiente de Curtose de Pearson e Coeficiente de Assimetria
round(kurtosis(df_amostra$NOTA_MT), 2)
# 2.56 - platicúrtica
round(skewness(df_amostra$NOTA_MT), 2)
# 0.17 - cauda do lado direito é relativamente maior que a do lado esquerdo


# Variável - NOTA_MT #
# Boxplot
df_amostra %>%
  ggplot(aes(x="", y = NOTA_MT)) +
  geom_boxplot(outlier.colour = "red", outlier.shape=8, outlier.size=4,
               fill="gray", color="black") +
  stat_summary(fun = mean, geom = "point", shape=23, size = 4) +
  labs(x ="Matemática", y = "Notas")+
  theme(panel.background = element_blank())
