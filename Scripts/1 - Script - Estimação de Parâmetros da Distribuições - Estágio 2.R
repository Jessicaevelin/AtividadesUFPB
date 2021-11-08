# Estimação de parâmetros das distribuições

# Geração de 100 sinistros conforme uma Normal de média 100 e desvio-padrão 50
sinistros <- rnorm(100, 100, 50)
# Imprimir os valores
print(sinistros)
# Criar um histograma
hist(sinistros)
hist(sinistros, breaks = 20)

# Carregar o pacote MASS
library(MASS)
# Gerar 100 valores por uma gama com beta = 2 e alfa = 0.01
x <-rgamma(100, 2, 0.01)
# Histograma
hist(x)
# Média
mean(x)
# Variância
var(x)
# Desvio-padrão
sd(x)
# quantis da distribuição
summary(x)

# Estimação pelo método dos momentos 
alpha <- mean(x)^2/var(x)
alpha
beta <- mean(x)/var(x)
beta
# Usando o método da máxima verossimilhança
fitdistr(x, "gamma")

# Função para estimar pelo método dos momentos para a Normal, Exponencial, Gama e Lognormal
mm <- function(x, dist){
  mu <- mean(x)
  sd <- sd(x)
  v <- var(x)
  if(dist == "normal"){
    print(mu)
    print(sd)
  }
  if(dist == "exponencial"){
    lambda <- 1/mu
    print(lambda)
  }
  if(dist == "gama"){
    alfa <- (mu^2)/v
    beta <- mu/v
    print(alfa)
    print(beta)
  }
  if(dist == "lognormal"){
    lnmu <- mean(log(x))
    lnsd <- sd(log(x))
    print(lnmu)
    print(lnsd)
  }
}

# Teste de estimação pelo método dos momentos
mm(x, "normal")
mm(x, "exponencial")
mm(x, "gama")
mm(x, "lognormal")

# Estimação pela Máxima verossimilhança
fitdistr(x, "normal")
fitdistr(x, "exponential")
fitdistr(x, "gamma")
fitdistr(x, "lognormal")



bn <- rnbinom(1000,  100, 0.99)
hist(bn)

summary(bn)
var(bn)



geo <- rgeom(1000, 0.9)
hist(geo)
summary(geo)
var(geo)




x <- c(100, 500, 2000, 10000)
f <- c(0.7, 0.2, 0.08, 0.02)
m1 <- sum(x*f)
m1
m2 <- sum(x^2*f)
m2
s2 <- m2 - m1^2
sqrt(s2)

lnx <- log(x)
lnm1 <- sum(lnx*f)
lnm1
lnm2 <- sum(lnx^2*f)
lns2 <- lnm2 - lnm1^2
sqrt(lns2)

1/530
  
m1^2/s2
m1/s2  
  

n <- 500 
p <- 0.07
EN <- n*p
EN
VN <- EN*(1-p)
VN
alfa <- 3
beta <- 0.0006
EX <- alfa/beta
EX
VX <- alfa/beta^2
VX
ES <- EX*EN
ES
VS <- EN*VX + VN*EX^2
VS
DPS <- sqrt(VS)
DPS
Z <- (229700 - ES)/DPS
Z
1 - pnorm(Z)


s <- c(5, 10, 60, 150, 1000)
f <- c(0.2, 0.4, 0.3, 0.09, 0.01)
lns <- log(s)
lns
sum(s*f)
sum(lns*f)
lns2 <- log(s)^2

z2 <- log(158) - sum(lns*f)/(sqrt(sum(lns2*f) - sum(lns*f)^2))
z2



##### 3ª Av. parte I #####
### 1.
# a) Poisson
n.sin <- c(6, 2, 3, 0, 2, 1, 2, 5, 1, 3)
lamba <- mean(n.sin)
# b) Binomial Negativa
sn <-  sum((n.sin - mean(n.sin))^2)/10
r <- (mean(n.sin)^2)/(sn - mean(n.sin))
r
p <- mean(n.sin)/sn
p
# c) Binomial com n = 10
p <- mean(n.sin)/10
p
### 2.
# a) P(N=0)
dpois(0, 15)
# b) P(N<=5)
ppois(5, 15)
# c) P(N=15)
dpois(15,15)
### 3. 
sin <- c(27,100, 150, 200, 400, 500, 550, 700, 800, 1500)
# a) Exponencial
alfa <- 1/mean(sin)
alfa
# b) Gama
m1 <- mean(sin)
m1
m2 <- mean(sin^2)
m2
alfa <- (m1^2)/(m2 - m1^2)
alfa  
beta <- m1/(m2 - m1^2)  
beta  
# c) Log Normal
lnsin <- log(sin)
mu <- mean(lnsin)
mu  
sigma <- sqrt(mean(lnsin^2)-mu^2)  
sigma  
### 4. P(S>1000)
z <- (log(1000)-mu)/sigma
1 - pnorm(z)  
### 5. 
lambda <- 100
teta <- 0.1  
alfa <- 0.008
EN <- lambda
VN <- lambda
EX <- 1/alfa
VX <- 1/(alfa^2)
# a) CVS
sdS <- sqrt(VN*(EX^2)+VX*EN)
ES <- EX*EN
CVS <- sdS/ES
CVS  
# b) CVR
ER <- 1/(1+teta)
ER  
CVR <- sqrt((VN*(EX^2)+VX*EN)/(EX*EN*(1+teta))^2)/ER
CVR
1# c) 
z <- (15000 - ES)/sdS
z
1 - pnorm(z)





