###### Aula de ajuste de distribuição ######

# Carregando os dados
sinistro <- read.csv("sinistro_automoveis.csv", sep = ",")
sinistro <- sinistro$x

# Histrograma
hist(sinistro)

# Estatísticas 
summary(sinistro)
media <- mean(sinistro)
desvio <- sd(sinistro)
n <- length(sinistro)

# Função para calcular diversas estatísticas
estatisticas <- function(x){
  x <- x[!is.na(x)] # isso serve para que ele omita as observações com NA
  me <- mean(x)
  med <- median(x)
  n <- length(x)
  s <- sd(x)
  mi <- min(x)
  ma <- max(x)
  q25 <- quantile(x, probs = 0.25)
  q75 <- quantile(x, probs = 0.75)
  return(c(n = n, media = me, mediana = med, desvio = s, 
           Q = q25, Q = q75, min = mi, max = ma))
}
# calculando as estatísticas descritivas
estatisticas(sinistro)

# Carregar pacote para realizar a estimação pela máxima-verossimilhança
library(fitdistrplus)

# Função do pacote que cria dois gráficos, o histograma e a acumulada
plotdist(sinistro, histo=TRUE, demp = TRUE)

# Função que demonstra o gráfico de Cullen e Frey cuja função é apresentar a proximidade da distribuição observada em relação às teóricas, tomando como base a curtose e assimetria
descdist(sinistro)

# Realizando as estimações
fitg <- fitdist(sinistro, "gamma")
fitg

# Se a estimação não convergir conforme acima, utilize parâmetros iniciais de sugestão diferentes de 0:
fitdist(sinistro, "gamma", start = list(shape = 0.06, rate = 0.001))

# Estimando os parâmetros para uma Weibull 
fitw <- fitdist(sinistro, "weibull")
fitw

# Estimando os parâmetros para uma Lognormal
fitln <- fitdist(sinistro, "lnorm")
fitln

# Visualizando a distribuição empírica em relação às estimadas
hist(sinistro, pch=20, breaks = 20, prob = TRUE, main = "")
curve(dgamma(x, shape = fitg$estimate[1], rate = fitg$estimate[2]), add = TRUE, col = "blue")
curve(dweibull(x, fitw$estimate[1], fitw$estimate[2]), add = TRUE, col = "green")
curve(dlnorm(x, fitln$estimate[1], fitln$estimate[2]), add = TRUE, col = "red")

# Criar quatro gráficos: Densidade, Acumulada, QQ-plot e PP-plot
par(mfrow=c(2,2))
plot.legend <- c("Gama","Weibull", "Lognormal")
denscomp(list(fitg,fitw, fitln), legendtext = plot.legend)
cdfcomp (list(fitg,fitw, fitln), legendtext = plot.legend)
qqcomp  (list(fitg,fitw,fitln), legendtext = plot.legend)
ppcomp  (list(fitg,fitw,fitln), legendtext = plot.legend)

# Para finalizar a análise, avaliaremos o Teste Kolmogorov-Sminorv e o critério informacional de Akaike com a função gofstat
gofstat(list(fitg,fitw, fitln), fitnames = c("Gama","Weibull", "LogNormal"))

