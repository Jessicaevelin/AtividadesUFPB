##### 1- B #####
#

# Severidade

X <- data.frame(x = c(0,0,10,20,30), px = c(0.3,0.4,0.18,0.08,0.04)); X

ex = sum(X$x * X$px) ; ex

ex2 = sum(X$x^2 * X$px) ; ex2

vx = ex2 - (ex)^2 ; vx

# Frequência

n <- 1000
p <- 0.8
q <- 0.2

en = (n*q)/p ; en

vn = (n*q)/p^2 ; vn

# Sinistro Agregado

es = en * ex ; es

vs = (ex)^2 * vn + en * vx ; vs

ds = sqrt( vs ) ; ds

# Prêmio Puro da resseguradora

# Z(1-a) = Z(1-0.025) = Z(0.975) = qnorm(0.975)

pp = es + qnorm(0.975) * ds ; pp

theta1 = (qnorm(0.975) * ds)/es ; theta1

#
###### 1 - C #####
#

# theta = margem de segurança
# LT = 20 , theta = 10%

theta2 <- 0.1

pp2 = es * (1 + theta2) ; pp2






#
#
#
#
#

Z <- data.frame(z = c(10,20,30,40,50), pz = c(0.3,0.4,0.18,0.08,0.04)); Z

prob.table(Z)






















