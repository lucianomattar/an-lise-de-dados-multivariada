#regressão logística
#Luciano Mattar

rm(list = ls()) #limpa as variãveis global environment

getwd() #get working directory

setwd("D:/Dropbox/curso multivariada R") #set working directory

## Carregando os pacotes
library(haven)
library(ggplot2)
library(stargazer)
library(dplyr)
library(car)
library(xtable)

## Lendo o banco de dados

## Regressão Logística 
#a função para regressão logistica glm() familia "binomial"
#serão feitas regressoes logisticas para as pnad96 e pnad2014 para comparação de resultados

## Modelo III banco -> PNAD 2014

pnad = read_sav("https://github.com/lucianomattar/curso-multivariada/blob/main/PNAD2014_30a50_novo4.sav?raw=true")

#criar dummy anos de estudo
pnad <- pnad %>% mutate(univer = case_when (
  anosesco >= 0 & anosesco <=  11 ~ 0,
  anosesco > 11 ~ 1)
)

reg3 = glm(univer ~ escmãe, data = pnad, family = "binomial")
summary(reg3)

#formula exp(b)/1+exp(b)
exp(coef(reg3)[1])/(1+exp(coef(reg3)[1]))*100 #escmãe = 0

exp(coef(reg3)[1]+coef(reg3)[2]*11)/(1+exp(coef(reg3)[1]+coef(reg3)[2]*11))*100 #escmãe = 11


## Modelo III banco -> PNAD96

pnad96 = read_sav("https://github.com/jonatasvarella/analisemultivariada/blob/master/PNAD96_25a60_PPGS_2019%20(1).sav?raw=true")

#V4703 esc
#V1219 esc mãe
pnad96 <- pnad96 %>% mutate(univer = case_when (
  V4703 >= 0 & V4703 <=  11 ~ 0,
  V4703 > 11 ~ 1)
)

reg3.2 = glm(univer ~ V1219, data = pnad96, family = "binomial")
summary(reg3.2)

exp(coef(reg3.2)[1])/(1+exp(coef(reg3.2)[1]))*100 #escmãe = 0

exp(coef(reg3.2)[1]+coef(reg3.2)[2]*11)/(1+exp(coef(reg3.2)[1]+coef(reg3.2)[2]*11))*100 #escmãe = 11

## Modelo IV banco, somente no PNAD96 utlizado aqui possui variável raça 
reg4 = glm(univer ~ V1219 + raçabin, data = pnad96, family = "binomial")
summary(reg4)

exp(coef(reg4)[1])/(1+exp(coef(reg4)[1]))*100 #escmãe = 0, raça = 0

exp(coef(reg4)[1]+coef(reg4)[3]*1)/(1+exp(coef(reg4)[1]+coef(reg4)[3]*1))*100 #escmãe = 0, raça = 1 (branco e amarelos)

## Analisando os modelos
#tabela stargazer html. ver arquivo de html (para ver no navegador e depois copiar) no diretorio de trabalho definido para o projeto
stargazer(reg3, reg3.2, reg4, type = "html", out = "reglog.html")

library(DescTools) #medidas de avaliação (pseudo-R2) dos modelos logísticos
cbind(`Modelo III` = PseudoR2(reg3, c("CoxSnell", "Nagelkerke", "McFadden")),
      `Modelo III.2`  = PseudoR2(reg3.2, c("CoxSnell", "Nagelkerke", "McFadden")),
      `Modelo IV`  = PseudoR2(reg4, c("CoxSnell", "Nagelkerke", "McFadden"))
)

#teste de anova para comparar modelos. aqui apenas será comparado os modelos feitos com a pnad96, 
#pois são modelos aninhados de um mesmo banco de dados
#faz a comparação entre os modelos, e se for estatisticamente significante, o modelo 2 é considerado melhor
anova(reg3.2, reg4, test = "Chisq")# o reg3.2 é significante melhor do que o reg4


