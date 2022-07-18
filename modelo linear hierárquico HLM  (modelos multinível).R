#Introduçãp regressão linear
#Luciano Mattar - monitor
#Curso análise de dados multivariadas Prof. Jorge Alexandre Neves

rm(list = ls()) #limpa as variáveis global environment

getwd() #get working directory

setwd("D:/Dropbox/curso multivariada R") #set working directory

#material de consulta para hlm em R, SPSS e HLM
#https://web.pdx.edu/~newsomj/mlrclass/

library(haven)#carrega bancos do spss

#carrega os bancos de dados. o formato dos arquivos de origem é sav do spss. são salvos em formato data.frame
nivel_estudante= read_sav("https://github.com/lucianomattar/curso-multivariada/blob/main/HSB1.SAV?raw=true")
nivel_escolas = read_sav("https://github.com/lucianomattar/curso-multivariada/blob/main/HSB2.SAV?raw=true")

hsb <- merge(x = nivel_estudante, y = nivel_escolas, by = "ID") #faz o merge pela variável de identificação entre os bancos

hsb$SECTOR <- as.factor(hsb$SECTOR)#transforma sector em categórica -> para plotagem
hsb$ID <- as.factor(hsb$ID)

library(dplyr)#cria média da renda por ano e atingido
mean_hsb <- group_by(hsb, SES, SECTOR) %>%
  summarise(MATHACH = mean(MATHACH, na.rm = TRUE))

library(ggplot2)
ggplot(mean_hsb, aes(x = SES, y = MATHACH, color = SECTOR) ) +
  geom_line(aes(group = SECTOR), size = 1) +
  ggtitle("Nota média de matemática relação ao SES dos pais do alunos\npara cada tipo de escola (pública=o, privada=1)")


library(nlme)#pacote para modelos multinível

# comparar com os resultados do hlm feito na sala de aula

names(hsb)#consulta quais são as variáveis do banco de dados

#### modelo 1 - anova / modelo vazio ou modelo de intercepto apenas #### 

# ID = 160, é a variável que representa cada umas das 160 escolas e o segundo nível

# '~ 1' estima o intercepto ou valor médio da variável dependente. é estatisticamente significativa para o modelo estimado
# 'radom = ~ 1|ID' é o efeito aleatório médio da escolas definidas pelos ID's
# o coeficiente do efeito fixo será o coeficiente do intercepto. para o modelo é 12.63 a média das notas em matemática

model.1 = lme(MATHACH ~ 1, random = ~ 1|ID, data = hsb)
summary(model.1) 

#com observações
ggplot(hsb, aes(x = SES, y = MATHACH, color = ID) ) +
  geom_point() +
  geom_line(aes(y = predict(model.1)), size = 1) +
  theme(legend.position="none")+ 
  ggtitle("Valores médios preditos para nota de matemática\nem relação ao SES dos pais do alunos para cada escola")

#sem observações
ggplot(hsb, aes(x=SES, y=MATHACH, color=ID))+
  geom_line(aes(y=predict(model.1)), size = 1)+
  theme(legend.position="none")+ 
  ggtitle("Valores médios preditos para nota de matemática\nem relação ao SES dos pais do alunos para cada escola")


#variance component
VarCorr(model.1)

#calcular o ICC (coeficiente de correlação intraclasse) a partir dos componenetes da variância
t0 <- as.numeric(VarCorr(model.1)[1,1])
sig2 <- as.numeric(VarCorr(model.1)[2,1])
t0/(t0+sig2) #0.1803518 ou 18,03% da variância da nota em matemática é 'entre' as escolas e o restante da variância ocorre 'dentro' das escolas


#### modelo 2 - ANCOVA ou ovariância ####

#mede a variabilidade da nota em matemática através das escolas controlando pelo status sócio-econômico dos pais dos alunos
# a variação entre SES e nota em matemática é constante entre todas as escolas
model.2 = lme(MATHACH ~ SES, random = ~ 1|ID, data = hsb)
summary(model.2)

VarCorr(model.2)
t0 <- as.numeric(VarCorr(model.2)[1,1])
sig2 <- as.numeric(VarCorr(model.2)[2,1])
t0/(t0+sig2)

#gráficos
#com observações
ggplot(hsb, aes(x = SES, y = MATHACH, color = ID) ) +
  geom_point() +
  geom_line(aes(y = predict(model.2)), size = 1) +
  theme(legend.position="none")+ 
  ggtitle("Valores médios preditos para nota de matemática\nem relação ao SES dos pais do alunos para cada escola")

#sem observações
ggplot(hsb, aes(x=SES, y=MATHACH, color=ID))+
  geom_line(aes(y=predict(model.2)), size = 1)+
  theme(legend.position="none")+ 
  ggtitle("Valores médios preditos para nota de matemática\nem relação ao SES dos pais do alunos para cada escola")

#### modelo 3 - constante como variável dependente ####

# será estimado se o SES e o tipo da escola (SECTOR) é preditivo da nota em matemática
# nesse modelo a curva do SES (slope) é constante entre as escolas
# variável SECTOR ou tipo de escola -> escola pública = 0 e católica(privada) = 1
model.3 = lme(MATHACH ~ SECTOR + SES, random = ~ 1|ID, data = hsb)
summary(model.3)

VarCorr(model.3)
t0 <- as.numeric(VarCorr(model.3)[1,1])
sig2 <- as.numeric(VarCorr(model.3)[2,1])
t0/(t0+sig2)

#com observações
ggplot(hsb, aes(x = SES, y = MATHACH, color = SECTOR) ) +
  geom_point() +
  geom_line(aes(y = predict(model.3) , group=ID), size = 1)+ 
  ggtitle("Valores médios preditos para nota de matemática\nem relação ao SES dos pais do alunos para escola privada=1 e pública=0")

#sem observações
ggplot(hsb, aes(x=SES, y=MATHACH, colour=SECTOR))+
  geom_line(aes(y=predict(model.3), group=ID), size =1)+ 
  ggtitle("Valores médios preditos para nota de matemática\nem relação ao SES dos pais do alunos para escola privada=1 e pública=0")


 #### modelo 4 - constante e inclinação como variáveis dependentes ####

# é estimado o efeito interativo entre SES e SECTOR sobre a nota em matemática
# no modelo interativo é permitido que o efeito de SES sobre a nota em matemática varie entre escolas públicas e católicas (privadas) ou
# se a relação entre nota em matemática e SES depende do tipo de escola
model.4 = lme(MATHACH ~ SECTOR + SES + (SECTOR*SES), random = ~ 1|ID, data = hsb)
summary(model.4)

VarCorr(model.4)
t0 <- as.numeric(VarCorr(model.4)[1,1])
sig2 <- as.numeric(VarCorr(model.4)[2,1])
t0/(t0+sig2)

#com observações
ggplot(hsb, aes(x = SES, y = MATHACH, color = SECTOR) ) +
  geom_point() +
  geom_line(aes(y = predict(model.4) , group=ID), size = 1)+ 
  ggtitle("Valores médios preditos para nota de matemática\nem relação ao SES dos pais do alunos em interaçao\ncom o tipo de escola (privada=1 e pública=0)")

#sem observações
ggplot(hsb, aes(x=SES, y=MATHACH, colour=SECTOR))+
  geom_line(aes(y=predict(model.4), group=ID), size =1)+ 
  ggtitle("Valores médios preditos para nota de matemática\nem relação ao SES dos pais do alunos em interaçao\ncom o tipo de escola (privada=1 e pública=0)")

#### modelo hlm temporal ####
temp = read_sav("https://github.com/lucianomattar/curso-multivariada/blob/main/rendimento_ano_atingidosBrumadinho.sav?raw=true")

names(temp)

temp$atingido <-as.factor(temp$atingido)
temp$municipi <- as.factor(temp$municipi)

#centralizar por grupo ano
temp$ano_g_m <- mean(temp$ano)
temp$ano_g_m <- temp$ano - temp$ano_g_m

#municipi é variável que defini o segundo nível, e atingido é a preditora de segundo nível
model.5 = lme(rendamed ~ atingido + ano_g_m + evento*atingido, random = ~  1 |municipi, 
              method="REML", data = temp)
summary(model.5)

VarCorr(model.5)
t0 <- as.numeric(VarCorr(model.5)[1,1])
sig2 <- as.numeric(VarCorr(model.5)[2,1])
t0/(t0+sig2)

#gráficos
#valor médio atingidos por ano
library(dplyr)#cria média da renda por ano e atingido
mean_temp <- group_by(temp, atingido, ano) %>%
  summarise(rendamed = mean(rendamed, na.rm = TRUE))

ggplot(mean_temp, aes(x = ano, y = rendamed, color = atingido) ) +
  geom_line(aes(group = atingido), size = 1) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1))+ 
  scale_y_continuous(breaks = seq(0, 2000, by = 100))+
  geom_text(aes(label=round(rendamed, digits = 2)), size = 4)+ 
  ggtitle("Rendimento médio das famílias atingidas e não atingidas")

#gráfico com valores preditos
#com observações
ggplot(temp, aes(x = ano, y = rendamed, color = atingido) ) +
  geom_point() +
  geom_line(aes(y = predict(model.5) , group=municipi), size = 1) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1))+ 
  scale_y_continuous(breaks = seq(0, 2000, by = 100))+ 
  ggtitle("Valores médios preditos para rendimento médio das famílias\npor ano entre atingindos e não-atingidos em Brumadinho")

#sem observações
ggplot(temp, aes(x=ano, y=rendamed, colour=atingido))+
  geom_line(aes(y=predict(model.5), group=municipi), size =1)+
  scale_x_continuous(breaks = seq(2010, 2020, by = 1))+ 
  scale_y_continuous(breaks = seq(0, 2000, by = 100))+ 
  ggtitle("Valores médios preditos para rendimento médio das famílias\npor ano entre atingindos e não-atingidos em Brumadinho")


#opção de pacote para estimação de modelos multinível
library(lme4)
library(lmerTest)#inseri p-value na função lmer()
model.6 = lmer(rendamed ~ atingido + ano_g_m + evento*atingido + (1 |municipi), 
               data = temp, REML = TRUE)
summary(model.6)
