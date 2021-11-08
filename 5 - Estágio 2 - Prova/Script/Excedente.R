# LEITURA DOS DADOS
grupo4 <- read.delim("C:/Users/Jessica/Desktop/grupo4.txt")
sin = grupo4$x
head(sin)
summary(sin)


# EXCEDENTE DE DANOS - PÁGINA 148
# QUESTÃO 4 -  a) e b) (apartir desses dá para fazer: e) e f))

#--------------#
# X RETIDO
# CENÁRIOS 1,2 E 3

# Limite técnico 1 = média
lim.tec1 = mean(sin) 
# Limite técnico 2 = 11976 (3 quartil)
lim.tec2 = 11976
# Limite técnico 3 = 17000 (aleatorio)
lim.tec3 = 17000

# o sinistro(x) retido (o valor que fica com a seguradora) é aquele que é igual ou menor que o limite técnico
sin.retido1 = ifelse(sin < lim.tec1, sin, lim.tec1) # tradução: se o sinistro for menor que o limite tecnico a seguradora paga o sinistro se for maior paga o limite tecnico e o resto vai para resseguradora
sin.retido2 = ifelse(sin < lim.tec2, sin, lim.tec2)
sin.retido3 = ifelse(sin < lim.tec3, sin, lim.tec3)
# Encontrar dos tres cenários/limites: Esperança, Variancia, a) Premio puro, a) premio comercial, e) P(S(ret)>Ppuro(ret)), f) P(S(res)>Ppuro(res))

#---------------# 
# X DA RESSEGURADORA

# o sinistro(x) da resseguradora é o (sinistro - limite técnico) ou seja aquele que é maior que o limite técnico onde a resseguradora paga a diferença do sinistro e do limite
sin.resseg1 = ifelse(sin < lim.tec1, 0, (sin - lim.tec1)) # tradução:  se o sinistro for menor que o limite técnico a responsabilidade da resseguradora é zero, e se for maior a resseguradora paga a diferença entre o sinistro e o limite técnico.
sin.resseg2 = ifelse(sin < lim.tec2, 0, (sin - lim.tec2))
sin.resseg3 = ifelse(sin < lim.tec3, 0, (sin - lim.tec3))
# Encontrar dos tres cenários/limites: Esperança, Variancia, a) Premio puro, a) premio comercial, e) P(S(ret)>Ppuro(ret)), f) P(S(res)>Ppuro(res))



#-----------------------------
#-----------------------------
#-----------------------------

# TESTANDO PARA SABER SE O CODIGO ESTÁ CORRETO
# O mean(sin.retido1) + mean(sin.resseg1) deve ser igual mean(sin), afinal um é complementar ao outro
mean(sin.retido1) + mean(sin.resseg1)
mean(sin)
