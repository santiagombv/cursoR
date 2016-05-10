##Caso 1. 
fum <- read.table("C:/RD/fumadores.txt", header = TRUE)
#  fum <- read.table(file=file.choose(), header = TRUE)
fum 

#inspección de datos 1: gráfico tipo Cleveland
plot(fum$ca.pulm)
plot(fum$alt)

#inspección de datos 2: gráfico bivariado
plot(fum$alt, fum$ca.pulm)

#modelo lineal (regresión)
fit<-lm(ca.pulm ~ alt, data=fum)

##componentes del objeto fit (algunos)
fit
fit$coefficients
fit$residuals[1:20] # sólo los primeros 20
fit$fitted[1:20] # sólo los primeros 20

##resúmenes de información
summary(fit)
anova(fit)

#diagnósticos gráficos. Layout dividirá la ventana gráfica en lo que 
#indique la matriz (4 en este caso, en 2 filas y 2 columnas). Para que la 
#ventana gráfica vuelva a la normalidad usaremos layout(1)
layout(matrix(1:4,2,2))
plot(fit)
layout(1)

#diagnósticos numéricos
shapiro.test(fit$residuals)

##Caso 2. 
data(InsectSprays)
head(InsectSprays)

plot(sqrt(count) ~ spray, data = InsectSprays)

#utilizando la function lm
fit.spray1 <- lm(sqrt(count) ~ spray, data = InsectSprays)
anova(fit.spray1)
summary(fit.spray1)
#utilizando la function aov
fit.spray2 <- aov(sqrt(count)~spray, data = InsectSprays)
summary(fit.spray2)

#diagnósticos gráficos
layout(matrix(1:4,2,2))
plot(fit.spray2)
layout(1)

#diagnósticos numéricos
shapiro.test(resid(fit.spray2))
bartlett.test(resid(fit.spray2)~InsectSprays$spray)

#Test de Tukey
#notar que trabaja sobre un objeto aov, no sobre un objeto lm
tuk<-TukeyHSD (fit.spray2)
tuk

#### END ####
