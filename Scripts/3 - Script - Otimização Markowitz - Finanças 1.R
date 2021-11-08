#Leitura do Banco de dados ( excel para .txt)

read.table("carteira.txt", header = T)
dados = read.table("carteira.txt", header = T)
dados




##INSTALAR E CARREGAR OS PACOTES 

require(fPortfolio)
library(fPortfolio)

require(timeSeries)
library(timeSeries)


##conversão no tipo de dados "timeSeries"
dados<-as.timeSeries(dados)

##############################################
##ESTATÍSTICAS DOS DADOS

##RETORNOS ESPERADOS
ret.esperados = colMeans(dados)
ret.esperados


#MATRIZ DE COVARIÂNCIAS
mat.cov = cov(dados)
mat.cov

################################################

##OTIMIZAÇÃO

##pesos dos ativos


##retorna o portfólio com a maior relação retorno/risco na fronteira eficiente

p1 = tangencyPortfolio(dados, spec = portfolioSpec(), constraints = "LongOnly")
p1


##retorna o portfólio com o risco mínimo na fronteira eficiente
p2 = minvariancePortfolio(dados, spec = portfolioSpec(), constraints = "LongOnly")
p2


## calculos para obtenção da fronteira eficiente
Frontier = portfolioFrontier(dados)

## Plotagem da fronteira eficiente no gráfico
frontierPlot(Frontier, col = c("blue", "orange"), pch = 19)

## adicinando informações ao gráfico
## pontos associados a possíveis carteiras
p3 = monteCarloPoints(Frontier, mcSteps = 5000, cex = 0.25, pch = 19)
p3
## mostrando o local da carteira que com proporções iguais em cada ativo
equalWeightsPoints(Frontier, pch = 15, col = "red")

## mostrando os pontos relativos a cada ativo individualmente
singleAssetPoints(Frontier, pch = 19, cex = 1.5, col = topo.colors(6))



