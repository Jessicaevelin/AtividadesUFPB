#Leitura do banco de dados

read.table("Markowitz.txt", header = TRUE)
dados = read.table("Markowitz.txt", header = TRUE)

##INSTALAR E CARREGAR OS PACOTES 

require(fPortfolio)
library(fPortfolio)

require(timeSeries)
library(timeSeries)


##conversão no tipo de dados "timeSeries"
dados<-as.timeSeries(dados)

##############################################
##ESTATATISCAS DOS DADOS

##RETORNOS ESPERADOS
ret.esperados = colMeans(dados)
ret.esperados


#MATRIZ DE COVARIÃNCIAS
mat.cov = cov(dados)
mat.cov

################################################

##OTIMIZAÇÃP

##pesos dos ativos


##retorna o portifolio com a maior relação retorno/risco na fronteira eficiente

p1 = tangencyPortfolio(dados, spec = portfolioSpec(), constraints = "LongOnly")
p1


##retorna o portfolio com o risco máximo na fronteira eficiente
p2 = minvariancePortfolio(dados, spec = portfolioSpec(), constraints = "LongOnly")
p2


## calculos para obtenção da fronteira eficiente
Frontier = portfolioFrontier(dados)

## Plotagem da fronteira eficiente no gráfico
frontierPlot(Frontier, col = c("blue", "orange"), pch = 19)

## adicinando informaÃ§Ãµes ao grÃ¡fico
## pontos associados a possÃ­veis carteiras
p3 = monteCarloPoints(Frontier, mcSteps = 5000, cex = 0.25, pch = 19)

## mostrando o local da carteira que com proporÃ§Ãµes iguais em cada ativo
equalWeightsPoints(Frontier, pch = 15, col = "red")

## mostrando os pontos relativos a cada ativo individualmente
singleAssetPoints(Frontier, pch = 19, cex = 1.5, col = topo.colors(6))



