int <- function(x){x*((1/50000)*exp(-(1/50000)*x))}
z <-integrate(int, lower = 0, upper = 100000)
y <- 29699.71
y + 100000*exp(-2)





























10,20,30,40,50
0,5,15,25,35
10,15,15,15,15
10-15,20-15,30-15,40-15,50-15
rm(list=ls(all=TRUE))

# Valores do sinistros

xres<-data.frame(x=c(0,5,15,25,35),px=c(0.3, 0.4, 0.18, 0.08, 0.04))
xres

ex = sum(xres$x * xres$px)
ex

ex2 = sum(xres$x^2 * xres$px)
ex2

vx = ex2 - ex^2
vx


# (N)

r = 1000
p = 0.8
q = 0.2

en = (r*q)/p
en

vn = (r*q)/p^2
vn

# S

es = en * ex
es

vs = ex^2 * vn + en * vx
vs

ds = sqrt(vs)
ds

pp = es + qnorm(0.975) * ds
pp