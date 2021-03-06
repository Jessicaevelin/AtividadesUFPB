#instalando os pacotes necessarios
require(quantmod)
library(quantmod)

require(PerformanceAnalytics)
library(PerformanceAnalytics)

ticker1 = 'GRND3.SA'
ticker2 = 'BBDC3.SA'
ticker3 = 'ITSA4.SA'
ticker4 = 'UGPA3.SA'
ticker5 = 'WEGE3.SA'

market.BR = '^BVSP'
market.EUA = '^DJI'
# ^DJI | ^GSPC
tickers.name = c(ticker1,ticker2,ticker3,ticker4,ticker5)
tickers.name1 = c(market.BR,market.EUA,ticker1,ticker2,ticker3,ticker4,ticker5)

date.first = as.Date('2012-09-01')
date.last = as.Date('2019-03-20')
periodicity = 'monthly'
periodicity2 = 12

rfEUA = 0.0266 # T-bond https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield
rfBR = 0.0972 # LTN http://www.tesouro.fazenda.gov.br/tesouro-direto-precos-e-taxas-dos-titulos
infEUA = 0.02 # http://www.inflation.eu/inflation-rates/united-states/historic-inflation/cpi-inflation-united-states.aspx
infBR = 0.04 # http://www.bcb.gov.br/pec/GCI/PORT/readout/readout.asp
prmBR = 0.0238 # EMIB/100 http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=40940&module=M

tickers.data <- getSymbols(tickers.name, from=date.first, to=date.last, periodicity=periodicity,auto.assign=T)
marketBR <- getSymbols.yahoo(market.BR, from=date.first, to=date.last, periodicity=periodicity,auto.assign=F)
marketEUA <- getSymbols(market.EUA, from=date.first, to=date.last, periodicity=periodicity,auto.assign=F)

tickers.data1 <- data.frame(marketBR[,4],marketEUA[,4],GRND3.SA[,4],BBDC3.SA[,4],ITSA4.SA[,4],UGPA3.SA[,4],WEGE3.SA[,4])
names(tickers.data1) <- c(tickers.name1)

tickers.r <- CalculateReturns(tickers.data1, method = 'log')
tickers.r <- tickers.r[-c(1),]

marketBR.pts.start <- data.frame(first(marketBR[,4],n=1))
marketBR.pts.final <- data.frame(last(marketBR[,4],n=1))
marketEUA.pts.start <- data.frame(first(marketEUA[,4],n=1))
marketEUA.pts.final <- data.frame(last(marketEUA[,4],n=1))

marketBR.r = ((marketBR.pts.final/marketBR.pts.start)^(1/(length(tickers.r[,1])/periodicity2)))-1
marketBR.r = marketBR.r[1,1]
marketEUA.r = ((marketEUA.pts.final/marketEUA.pts.start)^(1/(length(tickers.r[,1])/periodicity2)))-1
marketEUA.r = marketEUA.r[1,1]

marketBR.r.e = mean(tickers.r[,1])
marketEUA.r.e = mean(tickers.r[,2])
ticker1.r.e = mean(tickers.r[,3])
ticker2.r.e = mean(tickers.r[,4])
ticker3.r.e = mean(tickers.r[,5])
ticker4.r.e = mean(tickers.r[,6])
ticker5.r.e = mean(tickers.r[,7])

marketBR.r.sd = sd(tickers.r[,1])
marketEUA.r.sd = sd(tickers.r[,2])
ticker1.r.sd = sd(tickers.r[,3])
ticker2.r.sd = sd(tickers.r[,4])
ticker3.r.sd = sd(tickers.r[,5])
ticker4.r.sd = sd(tickers.r[,6])
ticker5.r.sd = sd(tickers.r[,7])

marketBR.sharpe = marketBR.r.e/marketBR.r.sd
marketEUA.sharpe = marketEUA.r.e/marketEUA.r.sd
ticker1.sharpe = ticker1.r.e/ticker1.r.sd
ticker2.sharpe = ticker2.r.e/ticker2.r.sd
ticker3.sharpe = ticker3.r.e/ticker3.r.sd
ticker4.sharpe = ticker4.r.e/ticker4.r.sd
ticker5.sharpe = ticker5.r.e/ticker5.r.sd

marketBR.beta = cov(tickers.r[,1],tickers.r[,1])/var(tickers.r[,1])
marketEUA.beta = cov(tickers.r[,1],tickers.r[,2])/var(tickers.r[,1])
ticker1.beta = cov(tickers.r[,1],tickers.r[,3])/var(tickers.r[,1])
ticker2.beta = cov(tickers.r[,1],tickers.r[,4])/var(tickers.r[,1])
ticker3.beta = cov(tickers.r[,1],tickers.r[,5])/var(tickers.r[,1])
ticker4.beta = cov(tickers.r[,1],tickers.r[,6])/var(tickers.r[,1])
ticker5.beta = cov(tickers.r[,1],tickers.r[,7])/var(tickers.r[,1])

ticker1.keBR = rfBR+(ticker1.beta*(marketBR.r-rfBR))
ticker2.keBR = rfBR+(ticker2.beta*(marketBR.r-rfBR))
ticker3.keBR = rfBR+(ticker3.beta*(marketBR.r-rfBR))
ticker4.keBR = rfBR+(ticker4.beta*(marketBR.r-rfBR))
ticker5.keBR = rfBR+(ticker5.beta*(marketBR.r-rfBR))

ticker1.keEUA = ((((rfEUA+(ticker1.beta*(marketEUA.r-rfEUA)))+prmBR)+1)*((1+infBR)/(1+infEUA)))-1
ticker2.keEUA = ((((rfEUA+(ticker2.beta*(marketEUA.r-rfEUA)))+prmBR)+1)*((1+infBR)/(1+infEUA)))-1
ticker3.keEUA = ((((rfEUA+(ticker3.beta*(marketEUA.r-rfEUA)))+prmBR)+1)*((1+infBR)/(1+infEUA)))-1
ticker4.keEUA = ((((rfEUA+(ticker4.beta*(marketEUA.r-rfEUA)))+prmBR)+1)*((1+infBR)/(1+infEUA)))-1
ticker5.keEUA = ((((rfEUA+(ticker5.beta*(marketEUA.r-rfEUA)))+prmBR)+1)*((1+infBR)/(1+infEUA)))-1


resumo = matrix(nrow = 6, ncol = 7)
resumo[1,1] = marketBR.r.e
resumo[2,1] = marketBR.r.sd
resumo[3,1] = marketBR.sharpe
resumo[4,1] = marketBR.beta
resumo[5,1] = marketBR.keBR
resumo[6,1] = marketBR.keEUA
resumo[1,2] = marketEUA.r.e
resumo[2,2] = marketEUA.r.sd
resumo[3,2] = marketEUA.sharpe
resumo[4,2] = marketEUA.beta
resumo[5,2] = marketEUA.keBR
resumo[6,2] = marketEUA.keEUA
resumo[1,3] = ticker1.r.e
resumo[2,3] = ticker1.r.sd
resumo[3,3] = ticker1.sharpe
resumo[4,3] = ticker1.beta
resumo[5,3] = ticker1.keBR
resumo[6,3] = ticker1.keEUA
resumo[1,4] = ticker2.r.e
resumo[2,4] = ticker2.r.sd
resumo[3,4] = ticker2.sharpe
resumo[4,4] = ticker2.beta
resumo[5,4] = ticker2.keBR
resumo[6,4] = ticker2.keEUA
resumo[1,5] = ticker3.r.e
resumo[2,5] = ticker3.r.sd
resumo[3,5] = ticker3.sharpe
resumo[4,5] = ticker3.beta
resumo[5,5] = ticker3.keBR
resumo[6,5] = ticker3.keEUA
resumo[1,6] = ticker4.r.e
resumo[2,6] = ticker4.r.sd
resumo[3,6] = ticker4.sharpe
resumo[4,6] = ticker4.beta
resumo[5,6] = ticker4.keBR
resumo[6,6] = ticker4.keEUA
resumo[1,7] = ticker5.r.e
resumo[2,7] = ticker5.r.sd
resumo[3,7] = ticker5.sharpe
resumo[4,7] = ticker5.beta
resumo[5,7] = ticker5.keBR
resumo[6,7] = ticker5.keEUA
resumo <- data.frame(resumo)
colnames (resumo) = c(tickers.name1)
row.names(resumo) = c('Re','Dp','Sharpe','Beta','KeBR','keEUA')
resumo
setwd ("c:/Users/Jessica/Desktop")
write.table(resumo, 'Finan�as.txt', sep=';')
