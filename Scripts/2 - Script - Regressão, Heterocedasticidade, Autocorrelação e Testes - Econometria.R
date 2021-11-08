## Instalando pacotes
install.packages("data.table")
install.packages("ggplot2")
install.packages("normtest")
install.packages("tseries")
install.packages("vars")
install.packages("fBasics")
install.packages("het.test")


## Iniciando os arquivos instalados

library(data.table)
library(ggplot2)
library(normtest)
library(tseries)
library(vars)
library(fBasics)
library(het.test)


#Definição do diretório de trabalho
getwd()
setwd("C:/Users/marcu/Desktop/Estagio")
dir()

#Importando base de dados
dados<-read.table("base_dados.txt", head=T)
dados

class(dados)

#Separando as variáveis da tabela

BM<-dados[,1]
BM

D<-dados[,2]
D

RP<-dados[,3]
RP

R<-dados[,4]
R

#Transformando em série temporal
BM<-ts(BM, frequency=12, start=c(2000,1))
BM

D<-ts(D, frequency=12, start=c(2000,1))
D
       
RP<-ts(RP, frequency=12, start=c(2000,1))
RP

R<-ts(R, frequency=12, start=c(2000,1))
R

dadosTS<-ts(dados, frequency=12, start=c(2000,1))
dadosTS

#Criação de gráficos
options(scipen=999)

plot.ts(BM)
plot.ts(D)
plot.ts(RP)
plot.ts(R)

?plot.ts
plot.ts(BM, main='Gráfico 1: Base Monetária, 2000-2017', col='blue', xlab="mês", ylab="R$ Milhões")
plot.ts(D, main='Gráfico 2: DBGG, 2000-2017 ', col='red', xlab="mês", ylab="R$ Milhões")
plot.ts(RP, main='Gráfico 3: Resultado Primário, 2000-2017', col='green', xlab="mês", ylab="R$ Milhões")
plot.ts(R, main='Gráfico 4: Pagamento de Juros, 2000-2017', col='black', xlab="mês", ylab="R$ Milhões")

#Estatística descritiva

summary(BM)
summary(D)
summary(RP)
summary(R)

#Média
media_BM<-mean(BM)
media_BM

media_D<-mean(D)
media_D

media_RP<-mean(RP)
media_RP

media_R<-mean(R)
media_R

#Mediana
med_BM<-median(BM)
med_BM

med_D<-median(D)
med_D

med_RP<-median(RP)
med_RP

med_R<-median(R)
med_R

#Variância
var_BM<-var(BM)
var_BM

var_D<-var(D)
var_D

var_RP<-var(RP)
var_RP

var_R<-var(R)
var_R

#Desvio Padrão
des_BM<-sd(BM)
des_BM

des_D<-sd(D)
des_D

des_RP<-sd(RP)
des_RP

des_R<-sd(R)
des_R

#Coeficiente de Variação

CV_BM<-(des_BM/media_BM)*100
CV_BM

CV_D<-(des_D/media_D)*100
CV_D

CV_RP<-(des_RP/media_RP)*100
CV_RP

CV_R<-(des_R/media_R)*100
CV_R

#Histograma
?hist
hist(BM, breaks="sturges", col="blue", ylab="frequencia")
hist(D, breaks="sturges", col="blue", main="", ylab="frequencia")
hist(RP, breaks="sturges", col="blue", main="", ylab="frequencia")
hist(R, breaks="sturges", col="blue", main="", ylab="frequencia")

#Teste de Normalidade - Jarque-Bera (Se p for muito baixo, pode-se rejeitar a hipótese de que a distribuição é normal)

JB_BM<-jb.norm.test(BM)
JB_BM

JB_D<-jb.norm.test(D)
JB_D

JB_RP<-jb.norm.test(RP)
JB_RP

JB_R<-jb.norm.test(R)
JB_R

#Coeficiente de Correlação
cor.test(BM,D)
cor.test(BM,RP)
cor.test(BM,R)
cor.test(D,RP)
cor.test(D,R)
cor.test(RP,R)

#Regressão Linear - MQO
regressao<-lm(D~RP+R+BM)
regressao
summary(regressao)

#Análise dos Resíduos
residuos<-residuals(regressao)
residuos

plot(residuos,type="l")

JB_residuos <- jb.norm.test(residuos)
JB_residuos

#Heterocedasticidade (análise da variância do termo de erro) 
?gqtest
gqtest(regressao)

het<-subset(dados,select=c(D,RP,R,BM))
var=VAR(het,p=1,type="none")
whites.htest(var)

#Autocorrelação Serial 
dwtest(regressao)

bgtest(regressao)
bgtest(regressao, order=2)
bgtest(regressao, order=4)


#Normalidade
hist(residuos, nclass=8, col="blue")

jarque.bera.test(residuos)

JB_residuos <- jb.norm.test(residuos)
JB_residuos












