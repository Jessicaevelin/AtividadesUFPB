#Pacotes necessarios
library(readr)
library(fitdistrplus)
library(MASS)
library(actuar)

# Inportando os dados:

grupo4 <- read.table("grupo4.txt", h=T)
head(grupo4)

# Qual a dimens�o do data frame?

dim(grupo4)

# separado apenas o x do data frame

sin <- grupo4$x #sin = sinistros(x)
head(sin)

#----------------------------------------------------------------------#
# 1.
# a.
# estati�sticas descritivas

summary(sin) 

# b.
#histograma

hist(sin, main = "Histograma dos sinistros", xlab = "Valores dos sinistros", ylab = "Frequencia", breaks = 90)

#c.
# ajustes da distribuicao:

descdist(sin) 

# Estimando os parametros para normal,gamma,lognormal,weibull e pareto:
# Gamma e Pareto precisam do pacote actuar

fit.normal <- fitdist(sin, "norm")
fit.gamma <- fitdist(sin, distr = "gamma", method = "mle", lower = c(0, 0), start = list(scale = 1, shape = 1)) 
fit.lognormal <- fitdist(sin, "lnorm")
fit.weibull <- fitdist(sin, "weibull")
fit.pareto  <- fitdist(sin, "pareto", start = list(shape = 1, scale = 500)) 

# Parametros de cada distribuicao:

summary(fit.normal)
summary(fit.gamma)
summary(fit.lognormal)
summary(fit.weibull)
summary(fit.pareto)

# d.
# dados empiricos

plotdist(sin, histo = TRUE, demp = TRUE) 

# dados estimados

hist(sin, pch=20, breaks = 20, prob = TRUE, main = "")
curve(dnorm(x, fit.normal$estimate[1], fit.normal$estimate[2]), add = TRUE, col = "purple")
curve(dgamma(x, fit.gamma$estimate[1], fit.gamma$estimate[2]), add = TRUE, col = "blue")
curve(dlnorm(x, fit.lognormal$estimate[1], fit.lognormal$estimate[2]), add = TRUE, col = "red")
curve(dweibull(x, fit.weibull$estimate[1], fit.weibull$estimate[2]), add = TRUE, col = "orange")
curve(dpareto(x, fit.pareto$estimate[1], fit.pareto$estimate[2]), add = TRUE, col = "green")

# e.
#analisar qual distribuicao se parece com o histograma dos dados:

par(mfrow=c(2,2))
plot.legend <- c("Normal", "Gamma", "Lognormal", "Weibull", "Pareto")
denscomp(list(fit.normal, fit.gamma, fit.lognormal, fit.weibull, fit.pareto), legendtext = plot.legend)
cdfcomp (list(fit.normal, fit.gamma, fit.lognormal, fit.weibull, fit.pareto), legendtext = plot.legend)
qqcomp  (list(fit.normal, fit.gamma, fit.lognormal, fit.weibull, fit.pareto), legendtext = plot.legend)
ppcomp  (list(fit.normal, fit.gamma, fit.lognormal, fit.weibull, fit.pareto), legendtext = plot.legend)

#Analisar com base o Kolmogorov-Smirnov statistic e Akaike's Information Criterion:
gofstat(list(fit.normal, fit.gamma, fit.lognormal, fit.weibull, fit.pareto), fitnames = c("Normal", "Gamma", "Lognormal", "Weibull", "Pareto"))

# 2. 
# Dados

tetha = 0.1
lambda = 400
n = 10000 #( ????? E necessario usar? perguntar ao prof)

# Severidade "X"

esperanca.x <- mean(sin)
esperanca.x
variancia.x <- var(sin)
variancia.x
desvio.x <- sd(sin)
desvio.x
coefvar.x <- desvio.x / esperanca.x
coefvar.x

# Frequencia "N" ~ Poisson (lambda = 400 )

lambda = 400
esperanca.n = variancia.n = lambda
esperanca.n
variancia.n

# Sinistro Agregado

esperanca.s = esperanca.n * esperanca.x
esperanca.s
variancia.s = esperanca.n * variancia.x + (esperanca.x) ^ 2 * variancia.n
variancia.s

# Premio Puro
premiopuro = esperanca.s * (1 + tetha)
premiopuro

#-------------------------------------------------------------------------------------#
# 3.
alpha = 0.35
premiocomercial = premiopuro / (1 - alpha)
premiocomercial

#-------------------------------------------------------------------------------------#

# 4.
# a. 
# Cenario 1
quota = 0.5
# Cenario 2
quota2 = 0.25
# Cenario 3
quota3 = 0.75

#Premio de resseguro retido(puro e comercial)
##Cen�rio 1
PPresseg = premiopuro*quota
PPresseg
PPRet = premiopuro*(1 - quota)
PPRet
PCresseg = premiocomercial*quota
PCresseg
PCRet = premiocomercial*(1-quota)
PCRet

##Cen�rio 2
PPresseg2 = premiopuro*quota2
PPresseg2
PPRet2 = premiopuro*(1 - quota2)
PPRet2
PCresseg2 = premiocomercial*quota2
PCresseg2
PCRet2 = premiocomercial*(1-quota2)
PCRet2


##Cen�rio 3
PPresseg3 = premiopuro*quota3
PPresseg3
PPRet3 = premiopuro*(1 - quota3)
PPRet3
PCresseg3 = premiocomercial*quota3
PCresseg3
PCRet3 = premiocomercial*(1-quota3)
PCRet3

#------------------------------------------------------------------------------------#
# b. Pr�mio Puro da Resseguradora e Pr�mio Comercial da Resseguradora considerando o carregamento
#para despesas da resseguradora de 20%.
teta = 0.2
#Cen�rio 1

PPressegcarreg = PPresseg * (1+teta)
PPressegcarreg
PCressegcarreg = PCresseg * (1+teta)
PCressegcarreg

#Cen�rio 2

PPressegcarreg2 = PPresseg2 * (1+teta)
PPressegcarreg2
PCressegcarreg2 = PCresseg2 * (1+teta)
PCressegcarreg2

#Cen�rio 3

PPressegcarreg3 = PPresseg3 * (1+teta)
PPressegcarreg3
PCressegcarreg3 = PCresseg3 * (1+teta)
PCressegcarreg3

#----------------------------------------------------------------------------------$
# c. allan

#-----------------------------------------------------#
# d.
# Distribuicao do sinistro de responsabilidade da resseguradora.
# Como e a distribuicao da resseguradora o valor do sinistro sera multiplicado a porcentagem que representa a resseguradora.
# lambda = 400
# theta = 0.1
# Fx.resseguradora = ppois(x ,lambda) * quota , x>0
#
#------------------------------------------------------#
#------------------------------------------------------# CEN?RIO 1 
#------------------------------------------------------#
#
# e. CENARIO 1 -  Pagina 146 do ferreira
# Probabilidade dos sinistros retidos superarem o premio puro retido.

# Sinistros retidos na seguradora
sinret = sin * (1 - quota)  # aplicando a (1 - quota) usada no contrato eu irei encontrar os sinistros retidos na seguradora.
head(sinret) # as 5 primeiras linhas

# Esperanca - sin.ret
esperanca.sinret = mean(sinret)
esperanca.sinret

# Esperanca^2 - sin.ret 
esperanca2.sinret = mean((sinret)^2)
esperanca2.sinret

# Variancia - sin.ret
variancia.sinret = var(sinret)
variancia.sinret

# Desvio - sin.ret
desvio.sinret = sd(sinret)
desvio.sinret

# Sinistro agregado - sin.ret.
esperanca.s.sinret = lambda * esperanca.sinret
esperanca.s.sinret

# Variancia do sinistro agregado - sin.ret
variancia.s.sinret = lambda * esperanca2.sinret
variancia.s.sinret

# Desvio do sinistro agregado - sin.ret
desvio.s.sinret = sqrt(variancia.s.sinret)
desvio.s.sinret

# Premio puro retido - sin.ret
premiopuro.sinret = esperanca.s.sinret * (1 + theta)
premiopuro.sinret

# Probabilidade de Sinistro retido > Premio puro retido
# P(S>P) = P(Z >(Pp(sret) - E(sret))/Desvio(sret)) = P(Z>x) = %
Probabilidade.sinret = 1 - pnorm(premiopuro.sinret, mean = esperanca.s.sinret, desvio.s.sinret)
Probabilidade.sinret
#.
#------------------------------------------------------------#
#.
# f. CENARIO 1
# Probabilidade dos sinistros da resseguradora superarem  o premio puro da resseguradora
# Sinistros repassados a resseguradora
sinres = sin * quota  # aplicando a quota usada no contrato eu irei encontrar os sinistros repassados a resseguradora.
head(sinres) # as 5 primeiras linhas

# Esperanca - sin.res
esperanca.sinres = mean(sinres)
esperanca.sinres

# Esperanca^2 - sin.res 
esperanca2.sinres = mean((sinres)^2)
esperanca2.sinres

# Variancia - sin.res
variancia.sinres = var(sinres)
variancia.sinres

# Desvio - sin.res
desvio.sinres = sd(sinres)
desvio.sinres

# Sinistro agregado - sin.res
esperanca.s.sinres = lambda * esperanca.sinres
esperanca.s.sinres

# Variancia do sinistro agregado - sin.res
variancia.s.sinres = lambda * esperanca2.sinres
variancia.s.sinres

# Desvio do sinistro agregado - sin.res
desvio.s.sinres = sqrt(variancia.s.sinres)
desvio.s.sinres

# Pr?mio puro "repassado" - sin.ret
premiopuro.sinres = esperanca.s.sinres * (1 + theta)
premiopuro.sinres

# Probabilidade de Sinistro da resseguradora  > Premio puro da resseguradora
# P(S>P) = P(Z >(Pp(sreS) - E(sres))/Desvio(sres)) = P(Z>x) = %
Probabilidade.sinres = 1 - pnorm(premiopuro.sinres, mean = esperanca.s.sinres, desvio.s.sinres)
Probabilidade.sinres

###### OS CODIGOS DE P. RETIDO E P. RESSEGURADORA ESTARAM CORRETOS SE A SOMA DOS DOIS FOREM IGUAIS AO P.PURO
premiopuro.sinret
premiopuro.sinres
premiopuro
premiopuro.sinret + premiopuro.sinres

#------------------------------------------------------#
#------------------------------------------------------# CEN?RIO 2 
#------------------------------------------------------#

#
# e. CENARIO 2 , quota2 = 0.25
# Probabilidade dos sinistros retidos superarem o premio puro retido.
quota2

# Sinistros retidos na seguradora
sinret2 = sin * (1 - quota2)  # aplicando a (1 - quota2) usada no contrato eu irei encontrar os sinistros retidos na seguradora.
head(sinret2) # as 5 primeiras linhas

# Esperanca - sin.ret
esperanca.sinret2 = mean(sinret2)
esperanca.sinret2

# Esperanca^2 - sin.ret 
esperanca2.sinret2 = mean((sinret2)^2)
esperanca2.sinret2

# Variancia - sin.ret
variancia.sinret2 = var(sinret2)
variancia.sinret2

# Desvio - sin.ret
desvio.sinret2 = sd(sinret2)
desvio.sinret2

# Sinistro agregado - sin.ret.
esperanca.s.sinret2 = lambda * esperanca.sinret2
esperanca.s.sinret2

# Variancia do sinistro agregado - sin.ret
variancia.s.sinret2 = lambda * esperanca2.sinret2
variancia.s.sinret2

# Desvio do sinistro agregado - sin.ret
desvio.s.sinret2 = sqrt(variancia.s.sinret2)
desvio.s.sinret2

# Premio puro retido - sin.ret
premiopuro.sinret2 = esperanca.s.sinret2 * (1 + theta)
premiopuro.sinret2

# Probabilidade de Sinistro retido > Premio puro retido
# P(S>P) = P(Z >(Pp(sret) - E(sret))/Desvio(sret)) = P(Z>x) = %
Probabilidade.sinret2 = 1 - pnorm(premiopuro.sinret2, mean = esperanca.s.sinret2, desvio.s.sinret2)
Probabilidade.sinret2
#.
#------------------------------------------------------------#
#.
# f. CENARIO 2
# Probabilidade dos sinistros da resseguradora superarem  o premio puro da resseguradora
# Sinistros repassados a resseguradora
sinres2 = sin * quota2  # aplicando a quota2 usada no contrato eu irei encontrar os sinistros repassados a resseguradora.
head(sinres2) # as 5 primeiras linhas

# Esperanca - sin.res
esperanca.sinres2 = mean(sinres2)
esperanca.sinres2

# Esperan?a^2 - sin.res 
esperanca2.sinres2 = mean((sinres2)^2)
esperanca2.sinres2

# Variancia - sin.res
variancia.sinres2 = var(sinres2)
variancia.sinres2

# Desvio - sin.res
desvio.sinres2 = sd(sinres2)
desvio.sinres2

# Sinistro agregado - sin.res
esperanca.s.sinres2 = lambda * esperanca.sinres2
esperanca.s.sinres2

# Variancia do sinistro agregado - sin.res
variancia.s.sinres2 = lambda * esperanca2.sinres2
variancia.s.sinres2

# Desvio do sinistro agregado - sin.res
desvio.s.sinres2 = sqrt(variancia.s.sinres2)
desvio.s.sinres2

# Pr?mio puro "repassado" a resseguradora - sin.ret
premiopuro.sinres2 = esperanca.s.sinres2 * (1 + theta)
premiopuro.sinres2

# Probabilidade de Sinistro da resseguradora  > Pr?mio puro da resseguradora
# P(S>P) = P(Z >(Pp(sreS) - E(sres))/Desvio(sres)) = P(Z>x) = %
Probabilidade.sinres2 = 1 - pnorm(premiopuro.sinres2, mean = esperanca.s.sinres2, desvio.s.sinres2)
Probabilidade.sinres2

###### OS CODIGOS DE P. RETIDO E P. RESSEGURADORA ESTARAM CORRETOS SE A SOMA DOS DOIS FOREM IGUAIS AO P.PURO
premiopuro.sinret2
premiopuro.sinres2
premiopuro
premiopuro.sinret2 + premiopuro.sinres2


#------------------------------------------------------#
#------------------------------------------------------# CENARIO 3 
#------------------------------------------------------#

# e. CENARIO 3 
# Probabilidade dos sinistros retidos superarem o premio puro retido.

# Sinistros retidos na seguradora
sinret3 = sin * (1 - quota3)  # aplicando a (1 - quota3) usada no contrato eu irei encontrar os sinistros retidos na seguradora.
head(sinret3) # as 5 primeiras linhas

# Esperan?a - sin.ret
esperanca.sinret3 = mean(sinret3)
esperanca.sinret3

# Esperan?a^2 - sin.ret 
esperanca2.sinret3 = mean((sinret3)^2)
esperanca2.sinret3

# Variancia - sin.ret
variancia.sinret3 = var(sinret3)
variancia.sinret3

# Desvio - sin.ret
desvio.sinret3 = sd(sinret3)
desvio.sinret3

# Sinistro agregado - sin.ret.
esperanca.s.sinret3 = lambda * esperanca.sinret3
esperanca.s.sinret3

# Variancia do sinistro agregado - sin.ret
variancia.s.sinret3 = lambda * esperanca2.sinret3
variancia.s.sinret3

# Desvio do sinistro agregado - sin.ret
desvio.s.sinret3 = sqrt(variancia.s.sinret3)
desvio.s.sinret3

# Pr?mio puro retido - sin.ret
premiopuro.sinret3 = esperanca.s.sinret3 * (1 + theta)
premiopuro.sinret3

# Probabilidade de Sinistro retido > Pr?mio puro retido
# P(S>P) = P(Z >(Pp(sret) - E(sret))/Desvio(sret)) = P(Z>x) = %
Probabilidade.sinret3 = 1 - pnorm(premiopuro.sinret3, mean = esperanca.s.sinret3, desvio.s.sinret3)
Probabilidade.sinret3
#.
#------------------------------------------------------------#
#.
# f. CENARIO 3
# Probabilidade dos sinistros da resseguradora superarem  o premio puro da resseguradora
# Sinistros repassados a resseguradora
sinres3 = sin * quota3  # aplicando a quota3 usada no contrato eu irei encontrar os sinistros repassados a resseguradora.
head(sinres3) # as 5 primeiras linhas

# Esperanca - sin.res
esperanca.sinres3 = mean(sinres3)
esperanca.sinres3

# Esperanca^2 - sin.res 
esperanca2.sinres3 = mean((sinres3)^2)
esperanca2.sinres3

# Variancia - sin.res
variancia.sinres3 = var(sinres3)
variancia.sinres3

# Desvio - sin.res
desvio.sinres3 = sd(sinres3)
desvio.sinres3

# Sinistro agregado - sin.res
esperanca.s.sinres3 = lambda * esperanca.sinres3
esperanca.s.sinres3

# Variancia do sinistro agregado - sin.res
variancia.s.sinres3 = lambda * esperanca2.sinres3
variancia.s.sinres3

# Desvio do sinistro agregado - sin.res
desvio.s.sinres3 = sqrt(variancia.s.sinres3)
desvio.s.sinres3

# Pr?mio puro "repassado" - sin.ret
premiopuro.sinres3 = esperanca.s.sinres3 * (1 + theta)
premiopuro.sinres3

# Probabilidade de Sinistro da resseguradora  > Premio puro da resseguradora
# P(S>P) = P(Z >(Pp(sreS) - E(sres))/Desvio(sres)) = P(Z>x) = %
probabilidade.sinres3 = 1 - pnorm(premiopuro.sinres3, mean = esperanca.s.sinres3, desvio.s.sinres3)
probabilidade.sinres3

###### OS CODIGOS DE P. RETIDO E P. RESSEGURADORA ESTARAM CORRETOS SE A SOMA DOS DOIS FOREM IGUAIS AO P.PURO
premiopuro.sinret3
premiopuro.sinres3
premiopuro
premiopuro.sinret3 + premiopuro.sinres3

