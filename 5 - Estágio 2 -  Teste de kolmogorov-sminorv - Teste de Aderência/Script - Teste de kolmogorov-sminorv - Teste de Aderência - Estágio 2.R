#### Teste de Kolmogorov-Sminorv ####

# A função que apllica o teste KS é ks.test(x, 'pdist', par1, ...)

## Exemplo: Teste, para o nível de significância de 5%, 
## se os dados abaixo provêm de uma mesma distribuição exponencial, com média 0.5.

dados <- c(0.378, 0.391, 0.458, 0.063, 0.009, 1,007, 0,470, 0,368, 0,831, 0,387, 0.228, 0.389, 0.627, 0.480, 0.093, 0.123, 0.089, 0.646, 0.093, 0.4)

#Resposta: 
## A exponencial possui E[X] = 1/alpha, logo como E[X] = 0.5, alpha = 2.
## Portanto, faremos o teste KS:
ks.test(dados, 'pexp', 2)

## Análise foi de que as distribuições são estatisticamente iguais. 

# Vamos realizar um teste.
## A partir dos valores dos sinistros (sin), 
## teste se a distribuição gama com parâmetros de forma (shape) = 4 e taxa (rate) = 0.002 ou escala = 500 
#taxa igual � 1/0.002 = 500 (Escala)
## se ajusta bem aos dados

sin <- c(1777.4871, 3085.1974, 772.0113, 2106.1301, 1418.5569, 2537.9169, 702.9835, 1505.3555, 2814.7499,1884.8414, 1107.7983, 2171.7479, 1837.6425, 1256.7446, 2218.2557, 1532.7183, 2729.2780, 1760.0429)

hist(sin, breaks = 10, prob = TRUE)
curve(dgamma(x, shape= 4, rate = 0.002),add = TRUE, col = "red")


#obrigatorio o prob igual a true
#breks = quebras, prob mostras a densidade inves da frequencia
#a cada x ele ir� calcular o gamma e criar uma linha encima do histograma
#resposta
ks.test(sin, 'pgamma', shape = 4, rate = 0.002,scale = 500)
#Como o P valor � 84% bem maior que 5%, ent�o � H0
#Como p-valor � maior que o valor que D, ent�o as distribui��es s�o iguais

#Teste KS
#Ho: As distribui��es s�o iguais
#H1: As distribui��es s�o diferentes

#pvalor> 5% n�o rejeita H0, ent�o � Ho
#pvalor< 5% rejeita H0, ent�o � H1
