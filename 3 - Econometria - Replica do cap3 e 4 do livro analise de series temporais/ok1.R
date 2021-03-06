


---
  title: "Econometria - Series Temporais"
output:
  pdf_document: default
html_document:
  df_print: paged
---
  
  Ativando os pacotes

```{r include=FALSE}
require(data.table)
require(ggplot2)
require(normtest)
require(tseries)
require(vars)
require(fBasics)
require(het.test)
```

Importando o banco de dados

```{r}
dados <- read.delim("D:/Google Drive/2 - Ufpb/P5/Econometria/Series/Dados/dados.txt", row.names=NULL, stringsAsFactors = FALSE)
dados < - dados
View(dados)

class(dados)
as.numeric(as.character(dados$x))

```

```{r}
dados

```

Separando as Variaveis da tabela
Uma forma de analisar variaveis do banco de formas separadas
Para simplificar, serão analisados apenas a (coluna2 até a coluna6) incluindo o ano(coluna2).

```{r}
Ano<-dados[,1]
DE<-dados[,2]
FP<-dados[,3]    
IC<-dados[,4]    
```
---
  Estatistica Descritiva
---
  
  
  ```{r}
summary(dados)
```

Medidas de Disperção

Variância

```{r}
var(DE)
var(FP)
var(IC)
```

Desvio Padrão

```{r}
sd(DE)
sd(FP)
sd(IC)
sd(SC)
```

Amplitude Total

```{r}
ampt_DE <- max(DE)-min(DE)
ampt_FP <- max(FP)-min(FP)
ampt_IC <- max(IC)-min(IC)
ampt_SC <- max(SC)-min(SC)

ampt_DE
ampt_FP
ampt_IC
ampt_SC
```

Amplitude Interquartil

```{r}
IQR(DE)
IQR(FP)
IQR(IC)
IQR(SC)

```

Coeficiente de variação

```{r}
sd(DE)/mean(DE)*100
sd(FP)/mean(FP)*100
sd(IC)/mean(IC)*100
sd(SC)/mean(SC)*100
sd(TD)/mean(TD)*100

```

Covariancia

```{r}
cov(dados)
```


---
  Gráficos
---
  
  
  Histograma

#criando o gráfico
```{r}
hist(DE, main = "Distribuição da frequencia Deposito em Poupança", 
     xlab = "Deposito em poupança",las=1 , col = "373")
hist(FP, main = "Distribuição da frequencia Folha de Pagamento", 
     xlab = "Folha de Pagamento",las=1 , col = "373")
hist(IC, main = "Distribuição da frequencia indice de confiança",
     xlab = "Indice de confiança",las=1 , col = "373")
hist(SC, main = "Distribuição da frequencia Selic", 
     xlab = "Selic",las=1 , col = "373")
hist(TD, main = "Distribuição da frequencia Taxa de Desemprego", 
     xlab = "Taxa de Desemprego",las=1 , col = "373")

```

Boxplot

#criando o gráfico
```{r}
boxplot(DE,
        xlab = "Deposito em poupança",las=1 , col = "373")
boxplot(FP,
        xlab = "Folha de Pagamento",las=1 , col = "373")
boxplot(IC,
        xlab = "indice de confiança",las=1 , col = "373")
boxplot(SC,
        xlab = "Selic",las=1 , col = "373")
boxplot(TD,
        xlab = "Taxa de Desemprego",las=1 , col = "373")

```

Dispersão

#criando o gráfico
```{r}
plot(DE, Ano,
     xlab = "Deposito em poupança", las=1, pch=19, col = "black")
plot(FP, Ano,
     xlab = "Folha de Pagamento", las=1, pch=19, col = "black")
plot(IC, Ano,
     xlab = "indice de confiança", las=1, pch=19, col = "black")
plot(SC, Ano,
     xlab = "Selic", las=1, pch=19, col = "black")
plot(TD, Ano,
     xlab = "Taxa de Desemprego", las=1, pch=19, col = "black")
```

Gráfico de Barras

#criando o gráfico
```{r}
barplot(DE, col = 373)
barplot(FP, col = 373)
barplot(IC, col = 373)
barplot(SC, col = 373)
barplot(TD, col = 373)
```

Ramos e Folhas

#criando o gráfico
```{r}
stem(DE)
stem(FP)
stem(IC)
stem(SC)
stem(TD)
```

Distribuição Empirica

#criando o gráfico
```{r echo=TRUE, message=FALSE, warning=FALSE}
nDE <- length(DE)
yDE <- (1:nDE)/nDE
deDE <- sort(DE)
plot(DE, yDE, type ="S", xlab ="Deposito em poupança", ylab ="Probabilidade", main ="Distribuição Empirica de Deposito em poupança")

nFP <- length(FP)
yFP <- (1:nFP)/nFP
deFP <- sort(FP)
plot(FP, yFP, type ="S", xlab ="Folha de Pagamento", ylab ="Probabilidade", main ="Distribuição Empirica de Folha de Pagamento")

nIC <- length(IC)
yIC <- (1:nIC)/nIC
deIC <- sort(IC)
plot(IC, yIC, type ="S", xlab ="indice de confiança", ylab ="Probabilidade", main ="Distribuição Empirica de indice de confiança")

nSC <- length(SC)
ySC <- (1:nSC)/nSC
deSC <- sort(SC)
plot(SC, ySC, type ="S", xlab ="Selic", ylab ="Probabilidade", main ="Distribuição Empirica Selic")

nTD <- length(TD)
yTD <- (1:TD)/TD
deTD <- sort(TD)
plot(TD, yTD, type ="S", xlab ="Taxa de Desemprego", ylab ="Probabilidade", main ="Distribuição Empirica de Taxa de Desemprego")
```
