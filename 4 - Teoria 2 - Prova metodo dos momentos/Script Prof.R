setwd("F:/grupo4")
dados=list()
length(dados)=12
for(i in 1:12)
{
dados[[i]]=read.csv(paste("grupo4am",i,".csv",sep=""))
}
dados[[1]]
dados
for(i in 1:12)
{
dados[[,i]]=read.csv(paste("grupo4am",i,".csv",sep=""))
}
dados=matrix()
for(i in 1:12)
{
dados[[,i]]=read.csv(paste("grupo4am",i,".csv",sep=""))
}
for(i in 1:12)
{
dados[,i]=read.csv(paste("grupo4am",i,".csv",sep=""))
}
dados=matrix()
length(dados)=12
for(i in 1:12)
{
dados[,i]=read.csv(paste("grupo4am",i,".csv",sep=";"))
}
dados=matrix()
for(i in 1:12)
{
dados[[,i]]=read.csv(paste("grupo4am",i,".csv",sep=""))
}
dados=matrix()
for(i in 1:12)
{
dados[[i]]=read.csv(paste("grupo4am",i,".csv",sep=""))
}
dados=list()
length(dados)=12
for(i in 1:12)
{
dados[[i]]=read.csv(paste("grupo4am",i,".csv",sep=""))
}
dados[[1]]
dados[[1]]
N=numeric(12)
N
S=numeric(12)
for(i in 1:12)
{
N[i]=dim(dados[[i]])[1]
S[i]=sum(dados[[i]][,2])
}
hist(S)
mean(S)
sd(S)
#premios com carregamento 1%
m=mean(S)
d=sd(S)
m=mean(S)
d=sd(S)
#premios com carregamento 1%
P=qnorm(1-0.01)*d+m
P
teta=(P/m)-1
teta
d/m
#premios com carregamento 5%
P=qnorm(1-0.05)*d+m
P
teta=(P/m)-1
teta
d/m
#premios com carregamento 10%
P=qnorm(1-0.1)*d+m
P
teta=(P/m)-1
teta
#METODO DO MOMENTO
#Valor p^
p=mean(N)/var(N)
p
#Valor do q^
q=1-mean(N)/var(N)
q
#ValoR do r^
r=mean(N)*p/q
r
#checar os momentos dos valores reais
Nsim=rnbinom(1000,r,p)
hist(Nsim)
mean(Nsim)
mean(N)
var(N)
var(Nsim)
##Simular os valores de S
#exemplo
Ssim=numeric(1000)
rnorm(14,0,1)
#concatenei (juntar todos os dados)
xdados=NULL
for(i in 1:12){
xdados=c(xdados,dados[[i]][,2])
}
hist(xdados)
?axis
hist(xdados)
title("nome")
hist(xdados,main="nome")
hist(S,main="nome")
setwd("F:/grupo4")
dados=list()
length(dados)=12
for(i in 1:12){
dados[[i]]=read.csv(paste("grupo4am",i,".csv",sep=""))
}
dados[[1]]

