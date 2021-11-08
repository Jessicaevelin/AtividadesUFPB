# Simulação de ruína

# Contar a quantidade de ruína nos 400 sinistros em um processo de ruína simulado
# Vamos estimar a probabilidade de ruína até acontecer o n-ésimo sinistro. 
# A distribuição valor de 1 sinistro é uma Gama com alpha = 2. 
# Poisson com lambda = 1

lambda <- 1
EX <- 1
theta <- 0.3
u <- 7.5
alpha <- 2
n <- 400
nSim <- 10000
set.seed(2)
c. <- (1+theta)*lambda*EX
N <- rep(Inf, nSim)

for (k in 1:nSim){
  Wi <- rexp(n)/lambda
  Ti <- cumsum(Wi)
  Xi <- gamma(n, shape = alpha)/alpha
  Si <- cumsum(Xi)
  Ui <- u + Ti*c. - Si
  ruin <- !all(Ui >= 0)
  if (ruin){ 
    N[k] <- min(which(Ui<0))
  }
}
N <- N[N<Inf]
N
length(N)
mean(N)
sd(N)
max(N)
length(N)/nSim

# O vetor Ui contém o excedente após o n-ésimo sinistro. 
# 255 a quantidade máxima de sinistros nos 745 cenários em que ocorreram a Ruína. 
# Média de 30.78792 e desvio de 24.71228
# Resultado: É improvável que a ruína ocorra após o 400º sinistro. 
# probabilidade aproximada de ruína é de 0.0745 ou 7.45%. 