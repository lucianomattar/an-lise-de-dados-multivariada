#Introduçãp regressão linear
#Luciano Mattar - monitor
#Curso análise de dados multivariadas Prof. Jorge Alexandre Neves

rm(list = ls()) #limpa as variáveis global environment

getwd() #get working directory

setwd("D:/Dropbox/curso multivariada R") #set working directory

library(haven)#carrega bancos do spss

#carrega os bancos de dados. o formato dos arquivos de origem é sav do spss. são salvos em formato data.frame
nivel_estudante= read_sav("https://github.com/lucianomattar/curso-multivariada/blob/main/HSB1.SAV?raw=true")
nivel_escolas = read_sav("https://github.com/lucianomattar/curso-multivariada/blob/main/HSB2.SAV?raw=true")

hsb <- merge(x = nivel_estudante, y = nivel_escolas, by = "ID") #faz o merge pela variável de identificação entre os bancos

library(nlme)#pacote para modelos multinível

names(hsb)#consulta quais são as variáveis do banco de dados

#### modelo 1 - anova para modelo de intercepto #### 

#ID representa cada umas das observações de escola = 160
#modelo de intercepto ou vazio<- há diferença significativa entre escolas para a nota de matemática

# '~ 1' estima o intercepto ou efeito médio da variável dependente, e 'radom ~ 1|ID' é o efeito aleatório ou efeito médio do segundo nível definido pelos ID's das escolas
# são 160 ID's representando as 160 escolas da amostra
# o coeficiente do efeito fixo será o coeficiente do intercepto
model.1 = lme(MATHACH ~ 1, random = ~ 1|ID, data = hsb, na.action = "na.omit")
summary(model.1) 

#variance component
VarCorr(model.1)
#calcular o ICC (coeficiente de correlação intraclasse) a partir dos componenetes da variância
t0 <- as.numeric(VarCorr(model.1)[1,1])
sig2 <- as.numeric(VarCorr(model.1)[2,1])
t0/(t0+sig2)

#### modelo 2 - covariância ####

#ao invés do modelo vazio apenas com valor médio do intercepto, tem a variável preditora SES como efeito fixo junto ao intercepto
model.2 = lme(MATHACH ~ SES, random = ~ 1|ID, data = hsb, na.action = "na.omit")
summary(model.2)

VarCorr(model.2)
t0 <- as.numeric(VarCorr(model.2)[1,1])
sig2 <- as.numeric(VarCorr(model.2)[2,1])
t0/(t0+sig2)

#### modelo 3 - constante com variável dependente ####

#o variável SECTOR (escola pública = 0, privada = 1) do nível 2 é estimado como efeito fixo junto com a variável SES do nível 1
model.3 = lme(MATHACH ~ SES + SECTOR, random = ~ 1|ID, data = hsb, na.action = "na.omit")
summary(model.3)

VarCorr(model.3)
t0 <- as.numeric(VarCorr(model.3)[1,1])
sig2 <- as.numeric(VarCorr(model.3)[2,1])
t0/(t0+sig2)

#### modelo 4 - constante e inclinação como variáveis dependentes ####

#efeito interativo entre SES e SECTOR
model.4 = lme(MATHACH ~ SES + SECTOR + (SES*SECTOR), random = ~ 1|ID, data = hsb, na.action = "na.omit")
summary(model.4)

VarCorr(model.4)
t0 <- as.numeric(VarCorr(model.4)[1,1])
sig2 <- as.numeric(VarCorr(model.4)[2,1])
t0/(t0+sig2)
