
require(fitdistrplus)
require(actuar)

sinistro <- read.csv("sinistros.csv", header = TRUE, sep=",")

head(sinistro)

sinistro <- sinistro$charges

fitn <- fitdist(sinistro,"norm",method="mle")
fitu <- fitdist(sinistro,"unif",method="mle")
fite <- fitdist(sinistro,"exp",method="mme")
fitl <- fitdist(sinistro,"llogis",method="mle")
fitln <- fitdist(sinistro, "lnorm", method="mle")
fitg <- fitdist(sinistro, "gamma", method="mme")
fitw <- fitdist(sinistro, "weibull", method="mle")

hist(sinistro, pch=20, breaks = 20, prob = TRUE, main = "")
curve(dnorm(x, fitn$estimate[1], fitn$estimate[2]), add = TRUE, col = "blue")
curve(dunif(x, fitu$estimate[1], fitu$estimate[2]), add = TRUE, col = "red")
curve(dexp(x, fite$estimate[1], fite$estimate[2]), add = TRUE, col = "brown")
curve(dlogis(x, fitl$estimate[1], fitl$estimate[2]), add = TRUE, col = "purple")
curve(dlnorm(x, fitln$estimate[1], fitln$estimate[2]), add = TRUE, col = "green")
curve(dgamma(x, fitg$estimate[1], fitg$estimate[2]), add = TRUE, col = "yellow")
curve(dweibull(x, fitw$estimate[1], fitw$estimate[2]), add = TRUE, col = "orange")

plotdist(sinistro, histo = TRUE, demp = TRUE)

par(mfrow=c(2,2))
plot.legend <- c("Normal", "Uniforme", "Lognormal", "Weibull"))
denscomp(list(fitn, fitu, fitln, fitw), legendtext = plot.legend)
cdfcomp (list(fitn, fitu, fitln, fitw), legendtext = plot.legend)
qqcomp  (list(fitn, fitu, fitln, fitw), legendtext = plot.legend)
ppcomp  (list(fitn, fitu, fitln, fitw), legendtext = plot.legend)

par(mfrow=c(2,2))
plot.legend <- c("Exponencial", "Logistica", "Gamma")
denscomp(list(fite, fitl, fitg), legendtext = plot.legend)
cdfcomp (list(fite, fitl, fitg), legendtext = plot.legend)
qqcomp  (list(fite, fitl, fitg), legendtext = plot.legend)
ppcomp  (list(fite, fitl, fitg), legendtext = plot.legend)

library(actuar)

gofstat(list(fitn, fitu, fitln, fitw, fite, fitl, fitg), fitnames = c("Normal","Uniforme", "Lognormal", "Weibull", "Exponencial", "Logistica", "Gamma"))