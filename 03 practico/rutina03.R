##Caso 1. 
dat <- read.table("C:/RD/moscas.txt", header = TRUE)
#  dat <- read.table(file = file.choose(), header = TRUE)
plot(dat$torax)
plot(dat$vida)
plot(dat$trat,dat$vida)
plot(dat$torax,dat$vida)

#diferentes sumas de cuadrados
fit1.a <- lm(vida ~ trat*torax, data = dat)
fit1.b <- lm(vida ~ torax*trat, data = dat)
anova(fit1.a) #tipo I
anova(fit1.b)

library(car)              
Anova(fit1.a, type = "II")
Anova(fit1.b, type = "II")
Anova(fit1.a, type = "III")
Anova(fit1.b, type = "III")

summary(fit1.a)

#selección del modelo
fit2 <- lm(vida ~ trat + torax, data = dat)
Anova(fit2, type = "II")
fit3 <- lm(vida ~ trat, data =dat)
Anova(fit3, type = "II")

layout(matrix(1:4, 2, 2))
plot(fit3)
layout(1)
