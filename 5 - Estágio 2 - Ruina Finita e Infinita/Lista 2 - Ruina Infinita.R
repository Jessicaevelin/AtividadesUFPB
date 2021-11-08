## Lista Ruina Infinita ##
# 1ª. Determine o *coeficiente de ajustamento* para uma distribuição exponencial com parâmetro 0,08 e carregamento de segurança de 1,645. 

alpha1 <- 0.08
tetha1 <- 1.645

r1 = (tetha1*alpha1)/(1+tetha1); r1

# 2ª. Calcule o fundo de reserva para uma seguradora cuja distribuição de sinistros se comporta como uma Poisson Composta. O limite da probabilidade de ruína é de 0,02 e o coeficiente de ajustamento é de 0,15. 

r2 = 0.15
fi2 = 0.02

#Fórmula: fi(y2) = exp(-r2*y2)

y2 = -(log(fi2)/r2); y2

#3ª. Qual seria o fundo de reserva para uma seguradora cuja distribuição de sinistros se comporta como uma Poisson Composta? O limite da probabilidade de ruína é de 0,001 e o coeficiente de ajustamento é 0,04. 

r3 = 0.04
fi3 = 0.001

#Fórmula: fi3(y3) = exp(-r3*y3)

y3 = -(log(fi3)/r3); y3

#4ª. Calcule o fundo de reserva uma vez que a seguradora definiu a probabilidade de ruína em 2,5%, de modo que dentro de 4 anos a seguradora não se arruíne. Utilize as seguintes informações:

n4 = 300  #apolices
q4 = 0.07  #probilidade do sinistro
is4 = 1000  #valor da indenização
tetha4 = 0.1  #carregamento de segurança
T4 = 4  #tempo em anos
fi4 = 0.025
#Scol ~ poisson composta ~ Normal
#y4 = ?

lambda = n4 * q4 * T4 ; lambda
  
