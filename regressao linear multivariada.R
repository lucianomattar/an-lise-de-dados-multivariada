#Introduçãp regressão linear
#Luciano Mattar

rm(list = ls()) #limpa as variáveis global environment

getwd() #get working directory

setwd("D:/Dropbox/curso multivariada R") #set working directory


#introdução regressão linear

X = rnorm(100)      #gera dados aleatórios
e = rnorm(100)      #gera dados aleatórios

Y = 2 + 1.75*X + e  #Montamos o modelo com os coeficientes reais
hist(Y)             #plota um histograma para verificar a distribuição de Y
summary(Y)

cor(X,Y)

reg = lm(Y~X)       #monta o modelo
summary(reg)        #exibe os resultados do modelo
coef(reg)           #exibe só os coeficientes
confint(reg, level = 0.95)   #95% das vezes o verdadeiro valor do parametro estimado vai estar entre esse ic

# plotando a curva de regressão
plot(X,Y, pch = 19)  #plota o diagrama de dispersão
abline(reg = reg, lwd = 2, col = "red")  #plota a reta

#library(performance)  
#check_model(reg)

#### Regressão pnad ###########################################################

# Carregando os pacotes necessários
library(haven) #carrega função read_sav()
library(ggplot2)

#pnad = read_sav("PNAD2014_30a50_novo4.sav")

pnad = read_sav("https://github.com/lucianomattar/curso-multivariada/blob/main/PNAD2014_30a50_novo4.sav?raw=true")

names(pnad)  #nomes das variáveis
head(pnad, 10)   #primeiros casos
str(pnad)    #estrutura do objeto
View(pnad)   #visualiza o banco de dados

# plotando a dispersão do log da renda
ggplot(pnad, aes(x=lnrenda))+geom_histogram()

##exemplo
# Montando a regressão
reg = lm(anosesco~escmãe, data=pnad)

#Anova e resultados
anova(reg)

summary(reg)
confint(reg, level = .99)  #intervalo de confiança a 99%

plot(pnad$escmãe, pnad$anosesco)
abline(reg=reg, lwd=2, col="red")

ggplot(pnad, aes(x=escmãe, y=anosesco))+
  geom_point()+stat_smooth(method = "lm")

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg)


#### Regresssão linear simples ####
#### Modelo I #####
#Pedindo média de x e y
#ISEI-88 - International Socioeconomic Index

summary(pnad$isei88)      #média de y
summary(pnad$isei88pai)   #média de x

# Refazendo a regressão
reg = lm(isei88 ~ isei88pai, data=pnad)

#Analisando resultados e Anova
summary(reg)
anova(reg)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg)


#### Modelo II #####
#Pedindo média de x e y

summary(pnad$renda)      #média de y
summary(pnad$isei88)   #média de x

# Refazendo a regressão
reg2 = lm(renda ~ isei88, data=pnad)

#Analisando resultados e Anova
summary(reg2)
anova(reg2)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg2)


#### Modelo III #####
#Pedindo média de x e y

summary(pnad$lnrenda)      #média de y
summary(pnad$isei88)   #média de x

# Refazendo a regressão
reg3 = lm( lnrenda ~ isei88, data=pnad)

#Analisando resultados e Anova
summary(reg3)
anova(reg3)

coef(reg3)[1]        #Extraindo o primeiro coeficiente da reg
exp(coef(reg3)[1])   #calcula o exponencial do 1 coef

coef(reg3)[2]       #Extraindo o segundo coeficiente (da regressao)

# taxa simple = b1 * 100
coef(reg3)[2] * 100
#taxa composta = ( exp(b1)-1 ) * 100
(exp(coef(reg3)[2]) - 1) * 100


# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg3)


#### Modelo IV #####
## Criando o logaritmo da variável isei88

pnad$lnisei88 = log(pnad$isei88)

#Pedindo média de x e y
summary(pnad$lnrenda)      #média de y
summary(pnad$lnisei88)   #média de x

# Refazendo a regressão
reg4= lm(lnrenda ~ lnisei88, data=pnad)

#Analisando resultados e Anova
summary(reg4)
anova(reg4)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg4)


#### Modelo V #####
## Padronizando as variáveis

pnad_pad = data.frame(Y = pnad$isei88, X = pnad$anosesco)
head(pnad_pad)
# head(pnad_pad)
#    Y  X
# 1 55 15
# 2 25 11
# 3 17  0
# 4 17  8
# 5 17 11
# 6 25 11

pnad_pad$Y = (pnad_pad$Y - mean(pnad_pad$Y)) / sd(pnad_pad$Y)
pnad_pad$X = (pnad_pad$X - mean(pnad_pad$X)) / sd(pnad_pad$X)

head(pnad_pad)
#            Y          X
# 1  0.8788212  1.3084098
# 2 -0.7597625  0.3826646
# 3 -1.1967182 -2.1631348
# 4 -1.1967182 -0.3116443
# 5 -1.1967182  0.3826646
# 6 -0.7597625  0.3826646

pnad_pad <- data.frame(scale(pnad_pad, scale = TRUE)) #scale() centraliza (scale=FALSE) e padroniza (scale=TRUE)
head(pnad_pad)
#               Y          X
# [1,]  0.8788212  1.3084098
# [2,] -0.7597625  0.3826646
# [3,] -1.1967182 -2.1631348
# [4,] -1.1967182 -0.3116443
# [5,] -1.1967182  0.3826646
# [6,] -0.7597625  0.3826646

## Fazendo a regressão
reg_pad = lm(Y ~ X, data = pnad_pad)
summary(reg_pad)
anova(reg_pad)

plot(reg_pad)


#### regressão linear multipla ################################################

library(haven) #carrega função read_sav()
library(ggplot2)
library(car)
library(stargazer)

#### Modelo I ####
pnad = read_sav("https://github.com/lucianomattar/curso-multivariada/blob/main/PNAD2014_30a50_novo4.savãraw=true")

reg = lm(lnrenda ~ anosesco, data=pnad)
#check_model(reg)

#Analisando resultados e Anova
summary(reg)
anova(reg)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg)

#### Modelo II ####
reg2 = lm(lnrenda ~ anosesco + isei88pai, data=pnad)

#Analisando resultados e Anova
summary(reg2)
anova(reg2)
vif(reg2) #car #valores acima entre 5 e 10 devem ser tomados com cautela e rejeitados acima de 10.

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg2)


#### Modelo III ####
reg3 = lm(lnrenda ~ anosesco + isei88pai + isei88, data=pnad)

#Analisando resultados e Anova
summary(reg3)
anova(reg3)
vif(reg3)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg3)


#### Modelo IV ####  
## Padronizando as variáveis

pnad_pad = pnad

pnad_pad$lnrenda_pad = (pnad_pad$lnrenda - mean(pnad_pad$lnrenda)) / sd(pnad_pad$lnrenda)
pnad_pad$anosesco_pad = (pnad_pad$anosesco - mean(pnad_pad$anosesco)) / sd(pnad_pad$anosesco)
pnad_pad$isei88pai_pad = (pnad_pad$isei88pai - mean(pnad_pad$isei88pai)) / sd(pnad_pad$isei88pai)
pnad_pad$isei88_pad = (pnad_pad$isei88 - mean(pnad_pad$isei88)) / sd(pnad_pad$isei88)

#### Modelo padronizado ####

reg4 = lm(lnrenda_pad ~ anosesco_pad + isei88pai_pad + isei88_pad, data = pnad_pad)

summary(reg4)
anova(reg4)
vif(reg4)

## tabela com os modelos
stargazer(reg, reg2, reg3,reg4,  type="html", out="tabela.html")

###############################################################################

library(haven) #carrega função read_sav()
library(ggplot2)
library(car)
library(tidyverse) #dplyr

#### regressão linar multipla ####

pnad96 = read_sav("https://github.com/lucianomattar/curso-multivariada/blob/main/PNAD96_25a60_PPGS_2019%20(1).sav?raw=true")
head(pnad96, 10)

#### Modelo I ####

reg = lm(lnrenda ~ anosesco + raçabin, data = pnad96)
summary(reg)
coef(reg)

#Plotando as curvas de regressão para cada categoria de raça
plot(pnad96$anosesco, pnad96$lnrenda)
abline(coef(reg)[1],coef(reg)[2], lwd=2, col="red")
abline((coef(reg)[1]+coef(reg)[3]), coef(reg)[2], lwd=2, col="blue")

#### Modelo II - Utilizando termo interativo ####

reg2 = lm(lnrenda ~ anosesco + raçabin + raçaesco, data = pnad96) #raça binária: brancos + amarelos = 1
summary(reg2)
coef(reg2)

reg2 = lm(lnrenda ~ anosesco + raçabin + (anosesco*raçabin), data = pnad96) #alternativa para inserir termo interativo
summary(reg2)
coef(reg2)

plot(pnad96$anosesco, pnad96$lnrenda)
abline(coef(reg2)[1], coef(reg2)[2], lwd=2, col="red")
abline((coef(reg2)[1]+coef(reg2)[3]), (coef(reg2)[2]+coef(reg2)[4]), lwd=2, col="blue")

#### Modelo III - Variável região ####

##Recodificando as labels. 

pnad96$região = as.factor(pnad96$região) #factor() transforma em variaveis categorica
pnad96$região = factor(pnad96$região, levels = c("1", "2", "3", "4", "5"),
                       labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))

pnad96$região <- relevel(pnad96$região, ref="Nordeste") #define a categoria de referencia

reg3 = lm(lnrenda ~ anosesco + região, data=pnad96)
summary(reg3)
coef(reg3)

table(pnad$anosesco)

#### modelo IV - criar categorias ####

#anosesco categorica

pnad96$anosesco
table(pnad96$anosesco)

pnad96 <- pnad96 %>% mutate(anosesco_cat = case_when(
                 anosesco == 0 ~ "analf", #0
                 anosesco >= 1 & anosesco <= 5 ~ "fund1", #1 a 5
                 anosesco >= 6 & anosesco <= 9 ~ "fund2", #6 a 9 
                 anosesco >= 10 & anosesco <= 12 ~ "medio", #10 a 12
                 anosesco >= 13 ~ "superior"), # acima 13
                 anosesco_cat = factor(anosesco_cat, levels=c("analf","fund1","fund2","medio","superior"))              
  )

levels(pnad96$anosesco_cat)  

table(pnad96$anosesco_cat)

pnad96$anosesco_cat <- relevel(pnad96$anosesco_cat, ref="analf") #define a categoria de referencia

#regressão
reg4 = lm(lnrenda ~ anosesco_cat + raçabin + (anosesco_cat*raçabin), data = pnad96) #alternativa para inserir termo interativo
summary(reg4)
coef(reg4)

plot(pnad96$anosesco_cat, pnad96$lnrenda)

library(sjPlot)
plot_model(reg4, type = "pred", terms = c("anosesco_cat", "raçabin"))+ geom_line()

#### modelo V - criar dummies ####
#dummy
library(fastDummies)

#raça dummy
pnad96 <- dummy_cols(pnad96, select_columns = 'raçabin')

reg5.1 = lm(lnrenda ~ anosesco + raçabin_1 + (anosesco * raçabin_1), data = pnad96) 
summary(reg5.1)
coef(reg5.1)

plot_model(reg5.1, type = "pred", terms = c("anosesco", "raçabin_1"))

reg5.2 = lm(lnrenda ~ anosesco + raçabin_0 + (anosesco * raçabin_0), data = pnad96) 
summary(reg5.2)
coef(reg5.2)

plot_model(reg5.2, type = "pred", terms = c("anosesco", "raçabin_0"))

#anosesco dummy
pnad96 <- dummy_cols(pnad96, select_columns = 'anosesco_cat')

reg6 = lm(lnrenda ~ anosesco_cat_fund1 + 
                    anosesco_cat_fund2 +
                    anosesco_cat_medio +
                    anosesco_cat_superior + 
                    raçabin +
                    (anosesco_cat_fund1*raçabin) +
                    (anosesco_cat_fund2*raçabin) +
                    (anosesco_cat_medio*raçabin) +
                    (anosesco_cat_superior*raçabin), data = pnad96) 
summary(reg6)
coef(reg6)

fund1 <- plot_model(reg6, type = "pred", terms = c("anosesco_cat_fund1", "raçabin"))
fund2 <- plot_model(reg6, type = "pred", terms = c("anosesco_cat_fund2", "raçabin"))
medio <- plot_model(reg6, type = "pred", terms = c("anosesco_cat_medio", "raçabin"))
superior <- plot_model(reg6, type = "pred", terms = c("anosesco_cat_superior", "raçabin"))

library(gridExtra)

grid.arrange(fund1, fund2, medio, superior, nrow = 2)

## Modelo regressão linear polinomial
## Transformando idadecen em idade

pnad96$idade = pnad96$idadecen + (-min(pnad96$idadecen)) + 25 #retorna para 25 a 60 anos

## Calculando a média logaritmo da renda por idade

medias_renda = sapply(25:60, function(x) mean(pnad96$lnrenda[pnad96$idade == x]))

## Plotando o gráfico

ggplot(NULL, aes(y = medias_renda, x = 25:60)) +
  geom_line() + 
  ylab(label = "Média do logaritmo natural do rendimento do trabalho principal") +
  xlab(label = "Idade")

## Modelo I

## Criando a variável idade ao quadrado

pnad96$idade2 = pnad96$idade^2
summary(pnad96$idade2)

## Modelo de regressão polinomial
reg = lm(lnrenda ~ idade + idade2, data=pnad96)
summary(reg)
vif(reg)  #verifica colinearidade

## Modelo II - utilizando a idade centralizada

reg2 = lm(lnrenda ~ idadecen + idadecen2 , data=pnad96)
summary(reg2)
vif(reg2) #idade centralizada corrige a colineariedade
