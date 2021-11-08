# 1° Lista de Exercicios - Distribuições de sinistros - Estágio

## Questão 1
n = 100
p = 0.2
q = 1 - p

# a) En e Vn
En = n * p
En
Vn = n * p * q
Vn

# b) P(N = En)
dbinom(x = En, size = n, prob = p)

# c) P(N = 1.5*Vn)
dbinom(x = 1.5*Vn, size = n, prob = p)

## Questão 2
lambda = 3

# a) P(N = 0)
dpois(x = 0,lambda = 3)

# b) P(N<=4)
ppois(q =4, lambda = 3)

# c) P(N>=4)
1 - ppois(q = 3, lambda = 3)

# d) P(N = 2*lambda)
dpois(x = 2*3, lambda = 3)

## Questão 3
N = 1000
P = 0.04
Q = 1-P

EN = N * P
EN
VN = N*P*Q
VN

EX = 1000
VX = 90^2

ES = EN*EX
ES
VS = EN*VX+EX^2*VN
VS

# Aproximação Binomial ~ Normal
# `P = P(S>50000)`
P = (50000-40000)/sqrt(38724000)
P

# P = P(Z > 1.606978)
# O valor de Z na tabela é:
pnorm(P)

#outra forma de fazer uma aproximação
pnorm (50000,40000,sqrt(38724000),lower.tail = TRUE)
