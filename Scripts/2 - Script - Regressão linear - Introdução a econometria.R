#SCRIPT#
#Valores das variaveis
x<-c(2,2,2,3,6,7,3)
y<-c(4,6,8,12,15,14,11)
x
y

#criando tabela
eco<-data.frame(y = y, x = x)
eco

#Estatistica Descritiva
summary(eco)

#Grafico
plot(y,x)

#Correla��o
cor(y,x)

#Teste de Hipoteses
cor.test(y,x)

#Regress�o e coeficientes
reg=lm(y~x)
reg

#Medidas descritivas(Erros-padr�o das estimativas,e R^2)
summary(reg)

#Anova - Mean Sq - Quadrado m�dio residual, que � estimatia para variancia dos erros s^2
anova(reg)

#Teste para NORMALIDADE (valores de p > 0,05 indicam dados normais)
shapiro.test(rstudent(reg))   ##teste de shapiro wilk (normalidade)

# An�lise visual para homogeneidade dos res�duos (visualmente eles devem se distribuir igualmente #abaixo e acima da linha
#analisar o grafico
plot(rstudent(reg) ~ fitted(reg), pch = 19)
abline(h = 0, lty = 2)

#Visualiza��o gr�fica lty � o tipo da linha 1: linha cont�nua; 2: linha descont�nua
#xlim e ylim alteram a escala
plot(y~x, ylim = c(0,15), xlim = c(0,7))

#interceptos e inclina��o fixadas manualmente
abline(4.028,1.672)

#interceptos e inclina��o automatico
abline(resmodelo,lty=2)


