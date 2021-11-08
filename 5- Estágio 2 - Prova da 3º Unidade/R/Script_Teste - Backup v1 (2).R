## Terceira Avaliação
#Pacotes
library(readr)
library(fitdistrplus)
library(MASS)
library(actuar)

## Analise do data frame:

# Importação£o dos dados:
grupo4 <- read_csv("D:/Google Drive/6 - R/5- Estágio 2 - Prova da 3º Unidade/Dados/grupo4.csv", 
                   col_types = cols(n = col_number(), x = col_number()))
head(grupo4)

# Qual a dimensãoo do data frame?
dim(grupo4)

# separado apenas o x do data frame
sin <- grupo4$x #sin = sinistros(x)
head(sin)

# 1.

# a) 

summary(sin) # estatísticas descritivas
sin_variancia <- var(sin); sin_variancia # variância
sin_desvio <- sd(sin); sin_desvio # desvio
sin_coefvar <- mean(sin)/sin_desvio ; sin_coefvar # coeficiente de variação dos sinistros

# b)

hist(sin, main = "Histograma dos sinistros", xlab = "Valores dos sinistros", ylab = "Frequência", breaks = 90)

# c) 

# ajustes da distribuição
descdist(sin) 

# Estimando os parâmetros para normal,gamma,lognormal,weibull e pareto
fit.normal <- fitdist(sin, "norm")
fit.gamma <- fitdist(sin, distr = "gamma", method = "mle", lower = c(0, 0), start = list(scale = 1, shape = 1)) # Precisa do pacote actuar
fit.lognormal <- fitdist(sin, "lnorm")
fit.weibull <- fitdist(sin, "weibull")
fit.pareto  <- fitdist(sin, "pareto", start = list(shape = 1, scale = 500)) # Precisa do pacote actuar

# Parâmetros de cada distribuição
summary(fit.normal)
summary(fit.gamma)
summary(fit.lognormal)
summary(fit.weibull)
summary(fit.pareto)

# d) Apresente os grÃ¡ficos dos modelos probabilÃ�sticos estimados em relaÃ§Ã£o Ã  distribuiÃ§Ã£o empÃ�rica.
# e) Indique qual o modelo que melhor se ajusta aos dados dos sinistros e explique o porquÃª.
