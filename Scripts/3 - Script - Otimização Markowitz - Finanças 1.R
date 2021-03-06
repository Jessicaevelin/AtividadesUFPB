#Leitura do Banco de dados ( excel para .txt)

read.table("carteira.txt", header = T)
dados = read.table("carteira.txt", header = T)
dados




##INSTALAR E CARREGAR OS PACOTES 

require(fPortfolio)
library(fPortfolio)

require(timeSeries)
library(timeSeries)


##convers�o no tipo de dados "timeSeries"
dados<-as.timeSeries(dados)

##############################################
##ESTAT�STICAS DOS DADOS

##RETORNOS ESPERADOS
ret.esperados = colMeans(dados)
ret.esperados


#MATRIZ DE COVARI�NCIAS
mat.cov = cov(dados)
mat.cov

################################################

##OTIMIZA��O

##pesos dos ativos


##retorna o portf�lio com a maior rela��o retorno/risco na fronteira eficiente

p1 = tangencyPortfolio(dados, spec = portfolioSpec(), constraints = "LongOnly")
p1


##retorna o portf�lio com o risco m�nimo na fronteira eficiente
p2 = minvariancePortfolio(dados, spec = portfolioSpec(), constraints = "LongOnly")
p2


## calculos para obten��o da fronteira eficiente
Frontier = portfolioFrontier(dados)

## Plotagem da fronteira eficiente no gr�fico
frontierPlot(Frontier, col = c("blue", "orange"), pch = 19)

## adicinando informa��es ao gr�fico
## pontos associados a poss�veis carteiras
p3 = monteCarloPoints(Frontier, mcSteps = 5000, cex = 0.25, pch = 19)
p3
## mostrando o local da carteira que com propor��es iguais em cada ativo
equalWeightsPoints(Frontier, pch = 15, col = "red")

## mostrando os pontos relativos a cada ativo individualmente
singleAssetPoints(Frontier, pch = 19, cex = 1.5, col = topo.colors(6))



