## ----eval=FALSE, include=FALSE-------------------------------------------
## install.packages("TTR")
## install.packages("forecast")
## install.packages("ggplot2")


## ----include=FALSE-------------------------------------------------------
require("TTR")
require("forecast")
require("ggplot2")


## ------------------------------------------------------------------------
dados <- read.delim2("D:/Google Drive/2 - Ufpb/P5/Econometria/Series/Dados/cap3.txt", row.names=1)
dados


## ------------------------------------------------------------------------
summary(naive(dados[,1],h=27))
summary(naive(dados[,2],h=27))
summary(naive(dados[,3],h=27))


## ------------------------------------------------------------------------
plot(naive(dados[,1],h=12),include = 200)
plot(naive(dados[,2],h=12),include = 200)
plot(naive(dados[,3],h=12),include = 200)


## ------------------------------------------------------------------------
summary(snaive(dados[,1],h=12))
summary(snaive(dados[,2],h=12))
summary(snaive(dados[,3],h=12))


## ------------------------------------------------------------------------
plot(snaive(dados[,1]))
plot(snaive(dados[,2]))
plot(snaive(dados[,3]))


## ------------------------------------------------------------------------
mm_centrada_DE6 <- ma(dados[,1],order=6)
mm_centrada_FP6 <- ma(dados[,2],order=6)
mm_centrada_TD6 <- ma(dados[,3],order=6)


## ------------------------------------------------------------------------
mm_centrada_DE12 <- ma(dados[,1],order=12)
mm_centrada_FP12 <- ma(dados[,2],order=12)
mm_centrada_TD12 <- ma(dados[,3],order=12)


## ------------------------------------------------------------------------
summary(mm_centrada_DE6)
summary(mm_centrada_FP6)
summary(mm_centrada_TD6)

## ------------------------------------------------------------------------
summary(mm_centrada_DE12)
summary(mm_centrada_FP12)
summary(mm_centrada_TD12)


## ------------------------------------------------------------------------
plot(dados[,1])
lines(mm_centrada_DE6,col="red",lty=5,lwd =2)
lines(mm_centrada_DE12,col="blue",lty=1 , lwd =3)
legend('topleft', legend=c("Deposito em poupança", "mm_centrada_DE6","mm_centrada_DE12"),
bty = "n",col=c("black","red", "blue"), lty=c(1,5,1), cex=0.8,lwd =c(1,2,3))


plot(dados[,2])
lines(mm_centrada_FP6,col="red",lty=5,lwd =2)
lines(mm_centrada_FP12,col="blue",lty=1 , lwd =3)
legend('topleft', legend=c("Folha de Pagamento", "mm_centrada_DE6","mm_centrada_DE12"),
bty = "n",col=c("black","red", "blue"), lty=c(1,5,1), cex=0.8,lwd =c(1,2,3))

plot(dados[,3])
lines(mm_centrada_TD6,col="red",lty=5,lwd =2)
lines(mm_centrada_TD12,col="blue",lty=1 , lwd =3)
legend('topleft', legend=c("Taxa de desemprego", "mm_centrada_DE6","mm_centrada_DE12"),
bty = "n",col=c("black","red", "blue"), lty=c(1,5,1), cex=0.8,lwd =c(1,2,3))


## ------------------------------------------------------------------------
plot(dados[,1])
mm_centrada_DE12 <- ma(dados[,1],order=12)
lines(mm_centrada_DE12,col="red")

plot(dados[,2])
mm_centrada_FP12 <- ma(dados[,2],order=12)
lines(mm_centrada_FP12,col="red")

plot(dados[,3])
mm_centrada_TD12 <- ma(dados[,3],order=12)
lines(mm_centrada_TD12,col="red")


## ------------------------------------------------------------------------
zDE <- dados[,1]
lDE <- 15
rDE <- 288
IC_DE <- function(zDE,rDE,lDE){
smadfDE <- SMA(zDE,rDE)
IC_IDE <- rep(smadfDE[length(zDE)],lDE) - 1.96*sd(zDE)/sqrt(rDE)
IC_SDE <- rep(smadfDE[length(zDE)],lDE) + 1.96*sd(zDE)/sqrt(rDE)
previsaoDE <- rep(smadf[length(zDE)],lDE)
cbind(IC_IDE,previsaoDE,IC_SDE)
}


zFP <- dados[,2]
lFP <- 15
r <- 288
IC_FP <- function(zFP,rFP,lFP){
smadfFP <- SMA(zFP,rFP)
IC_IFP <- rep(smadfFP[length(zFP)],l) - 1.96*sd(zFP)/sqrt(rFP)
IC_SFP <- rep(smadfFP[length(zFP)],l) + 1.96*sd(zFP)/sqrt(rFP)
previsaoFP <- rep(smadfFP[length(zFP)],lFP)
cbind(IC_IFP,previsaoFP,IC_SFP)
}

zTD <- dados[,3]
lTD <- 15
rTD <- 288
IC_TD <- function(zTD,rTD,lTD){
smadfTD <- SMA(zTD,rTD)
IC_ITD <- rep(smadfTD[length(zTD)],lTD) - 1.96*sd(zTD)/sqrt(rTD)
IC_STD <- rep(smadfTD[length(zTD)],lTD) + 1.96*sd(zTD)/sqrt(rTD)
previsaoTD <- rep(smadfTD[length(zTD)],lTD)
cbind(IC_ITD,previsaoTD,IC_STD)
}



## ------------------------------------------------------------------------
z<-c(dados[,1])
r<-12
l<-12

a <- IC_MMS(z,r,l)
b <- c(z,a[,1])
smadf <- SMA(z,r)
grafico <- ggplot(data=data.frame(b))+ geom_line(aes(c(1:length(b)),b))+
geom_smooth(data=data.frame(a),
aes(ymin = IC_I, ymax = IC_S,x = c((length(b)-l+1):length(b)),
y = Previsao), stat="identity")+
geom_line(data=data.frame(a),aes(c((length(b)-l+1):length(b)),IC_S))+
geom_line(data=data.frame(a),aes(c((length(b)-l+1):length(b)),IC_I))+
geom_line(data = data.frame(smadf),aes(c(1:(length(b)-l)),smadf),
na.rm = T,col="red")+
labs(title =" Depositos em poupança", x = "Tempo", y = "Dados")

grafico


## ------------------------------------------------------------------------
serieDE = ts(dados[,1],start = c(1991,1),frequency = 1)
serieFP = ts(dados[,2],start = c(2000,1),frequency = 1)
serieTD = ts(dados[,3],start = c(1991,1),frequency = 1)


## ------------------------------------------------------------------------
ajusteDE<-HoltWinters((serieDE), beta=FALSE, gamma=FALSE)
ajusteDE
ajusteFP<-HoltWinters((serieFP), beta=FALSE, gamma=FALSE)
ajusteFP
ajusteTD<-HoltWinters((serieTD), beta=FALSE, gamma=FALSE)
ajusteTD



## ------------------------------------------------------------------------
plot(ajusteDE)
plot(ajusteFP)
plot(ajusteTD)


## ----eval=FALSE, include=FALSE-------------------------------------------
## previsaoDE<-forecast.HoltWinters(ajusteDE)
## plot(previsaoDE)
## previsaoFP<-forecast.HoltWinters(ajusteFP)
## plot(previsaoFP)
## previsaoTD<-forecast.HoltWinters(ajusteTD)
## plot(previsaoTD)


## ------------------------------------------------------------------------
#Modelo para séries com tendência
#Suavização exponencial de Holt (SEH)
dados[,1]
dados[,2]
dados[,3]


## ------------------------------------------------------------------------
ajuste_com_tendenciaDE<-HoltWinters(dados[,1], gamma=FALSE)
ajuste_com_tendenciaDE
ajuste_com_tendenciaFP<-HoltWinters(dados[,2], gamma=FALSE)
ajuste_com_tendenciaFP
ajuste_com_tendenciaTD<-HoltWinters(dados[,3], gamma=FALSE)
ajuste_com_tendenciaTD



## ------------------------------------------------------------------------
plot(dados[,1])
lines(fitted(ajuste_com_tendenciaDE)[,1],col="red",lty=2,lwd =3)
legend('topleft', legend=c("Depositos em poupança", "ajuste_com_tendencia"),bty = "n",
       col=c("black","red"), lty=c(1,2), cex=0.8,lwd =c(1,3))
plot(dados[,2])
lines(fitted(ajuste_com_tendenciaFP)[,1],col="red",lty=2,lwd =3)
legend('topleft', legend=c("Folha de Pagamentos", "ajuste_com_tendencia"),bty = "n",
       col=c("black","red"), lty=c(1,2), cex=0.8,lwd =c(1,3))
plot(dados[,3])
lines(fitted(ajuste_com_tendenciaTD)[,1],col="red",lty=2,lwd =3)
legend('topleft', legend=c("Taxa de desemprego", "ajuste_com_tendencia"),bty = "n",
       col=c("black","red"), lty=c(1,2), cex=0.8,lwd =c(1,3))


## ----eval=FALSE, include=FALSE-------------------------------------------
## previsao_com_tendenciaDE<-forecast.HoltWinters(ajuste_com_tendenciaDE(dados[,1]))
## plot(previsao_com_tendenciaDE)
## previsao_com_tendenciaFP<-forecast.HoltWinters(ajuste_com_tendenciaFP(dados[,2]))
## plot(previsao_com_tendenciaFP)
## previsao_com_tendenciaTD<-forecast.HoltWinters(ajuste_com_tendenciaTD(dados[,3]))
## plot(previsao_com_tendenciaTD)
## 
## 


## ------------------------------------------------------------------------
dados[,1]
dados[,2]
dados[,3]


## ----eval=FALSE, include=FALSE-------------------------------------------
## ajuste_com_sazonalidadeDE<-HoltWinters(ts(dados[,1], start = 1995, frequency = 12, seasonal))
## ajuste_com_sazonalidadeDE
## ajuste_com_sazonalidadeFP<-HoltWinters(dados[,2])
## ajuste_com_sazonalidadeFP
## ajuste_com_sazonalidadeTD<-HoltWinters(dados[,3])
## ajuste_com_sazonalidadeTD


## ----eval=FALSE, include=FALSE-------------------------------------------
## plot(dados[,1])
## lines(fitted(ajuste_com_sazonalidadeDE)[,1]),col="red",lty=2,lwd =3),
## legend('topleft', legend=c("IDH 1991", "ajuste_com_sazonalidade"),
##        bty = "n',col=c("black","red"))
## 
## 
## plot(dados[,2])
## lines(fitted(ajuste_com_sazonalidadeFP)[,1]),col="red",lty=2,lwd =3),
## legend('topleft', legend=c("Folha de Pagamentos", "ajuste_com_sazonalidade"),
##        bty = "n',col=c("black","red"), lty=c(1,2), cex=0.8,lwd =c(1,3))
## 
## plot(dados[,3])
## lines(fitted(ajuste_com_sazonalidadeTD)[,1]),col="red",lty=2,lwd =3),
## legend('topleft', legend=c("Taxa de desemprego", "ajuste_com_sazonalidade"),
##        bty = "n',col=c("black","red"), lty=c(1,2), cex=0.8,lwd =c(1,3))
## 


## ----eval=FALSE, include=FALSE-------------------------------------------
## previsao_com_sazonalidadeDE<-forecast.HoltWinters(ajuste_com_sazonalidade(dados[,1]))
## plot(previsao_com_sazonalidadeDE)
## previsao_com_sazonalidadeFP<-forecast.HoltWinters(ajuste_com_sazonalidade(dados[,2]))
## plot(previsao_com_sazonalidadeFP)
## previsao_com_sazonalidadeTD<-forecast.HoltWinters(ajuste_com_sazonalidade(dados[,3]))
## plot(previsao_com_sazonalidadeTD)
## 
## 

