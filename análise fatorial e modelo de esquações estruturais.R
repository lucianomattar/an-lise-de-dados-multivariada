#Introduçãp regressão linear
#Luciano Mattar - monitor
#Curso análise de dados multivariadas Prof. Jorge Alexandre Neves

rm(list = ls()) #limpa as variáveis global environment

getwd() #get working directory

setwd("D:/Dropbox/curso multivariada R") #set working directory

library(haven)

PNAD_2014 <- read_dta("https://github.com/lucianomattar/curso-multivariada/blob/main/Pnad_2014?raw=true")

#### Análise fatorial exploratória ####

library(psych) 
# algumas vezes pode ocorrer erro com fa() por não estar instalado
# o pacote 'GPArotation'. se houver erro com essa descrição, instale.

cor_pnad <- cor(PNAD_2014[, 1:6])#cria uma martix de correlaçao com as 6 variaveis do banco pnad_2014

solucao = fa(r = cor_pnad, nfactors = 6)#sem método de rotação
solucao

solucao = fa(r = cor_pnad, nfactors = 6, rotate = "varimax")#com método varimax
solucao

#### Análise fatorial confirmatória ####

#   tutorial lavaan
#   https://lavaan.ugent.be/tutorial/tutorial.pdf

library(lavaan) #análise confirmatória e sem
library(semPlot)
library(tidySEM)
library(lavaanPlot)

# Livro_Modelo de equações estruturais, Jorge Alexandre Neves Barbosa
# https://repositorio.enap.gov.br/bitstream/1/3334/1/Livro_Modelo%20de%20equa%C3%A7%C3%B5es%20estruturais.pdf
# pag 51

afc_1 <- '
ose =~ iseopai + escpai + escmãe  
'

afc_um <- cfa(afc_1, data = PNAD_2014)
summary(afc_um, fit.measures = TRUE, standardized=TRUE)

semPaths(afc_um, "std",  layout = 'tree2', fixedStyle = 'white',
         what = "col", fade = FALSE, residuals = TRUE)

#### Modelo de equações estruturais ####

#pag 24
model_1 <- '
  # regressions
    esco ~ escmãe + escpai + iseopai 
    iseo ~ escmãe + escpai + iseopai + esco

  # residual correlations
    escmãe ~~ escpai
    escmãe ~~ iseopai
    escpai ~~ iseopai
'
fit_1 <- sem(model_1, data=PNAD_2014)
summary(fit_1, standardized=TRUE)

semPaths(fit_1, "std",  layout = 'tree2', fixedStyle = 'white',
         what = "col", fade = FALSE, residuals = TRUE)

#pag 38
model_2 <- '
  # regressions
    esco ~ escmãe + escpai + iseopai 
    iseo ~ iseopai + esco

  # residual correlations
    escmãe ~~ escpai
    escmãe ~~ iseopai
    escpai ~~ iseopai
'
fit_2 <- sem(model_2, data=PNAD_2014)
summary(fit_2, standardized=TRUE)

semPaths(fit_2, "std",  layout = 'tree2', fixedStyle = 'white',
         what = "col", fade = FALSE, residuals = TRUE)

#pag 64
model_3 <- '
  # measurement model                   
    dse =~ iseo + lnrenda
    
  # regressions
    dse ~ escmãe + escpai + iseopai + esco
    esco ~ escmãe + escpai + iseopai 

  # residual correlations
    escmãe ~~ escpai
    escmãe ~~ iseopai
    escpai ~~ iseopai
'
fit_3 <- sem(model_3, data=PNAD_2014)
summary(fit_3, standardized=TRUE)

semPaths(fit_3, "std",  layout = 'tree2', fixedStyle = 'white',
         what = "col", fade = FALSE, residuals = TRUE)

#### calcular efeitos indiretos e diretos ####
#exmeplo
model_4 <- ' 
  # efeito direto
    iseo ~ c*escmãe
    
  # mediação
    esco ~ a*escmãe
    iseo ~ b*esco
    
  # efeito indireto (a*b)
    efeito_ind_ab := a*b
    
  # efeito total
    efeito_total := c + (a*b)
  '
fit_4 <- sem(model_4, data=PNAD_2014)
summary(fit_4, standardized=TRUE)

semPaths(fit_4, "std",  layout = 'tree2', fixedStyle = 'white',
         what = "col", fade = FALSE, residuals = TRUE)
