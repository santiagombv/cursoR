##Caso 1.
# complete la ruta al directorio en ...
dat <- read.table(".../s_poly.txt", header = TRUE)

# exploración de los datos
layout(matrix(1:8,2,4))
plot(dat$azucar); plot(dat$largo.l.tot); plot(dat$largo.tubo) 
plot(dat$caliz.sup); plot(dat$caliz.med); plot(dat$caliz.inf) 
plot(dat$labio.sup); plot(dat$labio.inf)
layout(1)

# exploración bivariada de los datos
pairs(dat)

# Detección de la colinealidad
library(car)

# A) Gráficos SPLOM
scatterplotMatrix(dat, smooth = TRUE, regLine = TRUE)

# B) Matrices de correlación
CORR <- cor(dat[, c("largo.l.tot", "largo.tubo", "caliz.sup",
                    "caliz.med", "caliz.inf", "labio.sup", "labio.inf")], 
            use = "complete.obs") 
CORR

library(GGally)
ggcorr(dat, method = c("pairwise", "pearson"))

# C) Factores de inflación de la varianza
fit <- lm(azucar ~ largo.l.tot + largo.tubo + caliz.sup + caliz.med +
            caliz.inf + labio.sup + labio.inf, data = dat)
vif(fit)	

# Decisión: usar el criterio de r < 0.7 y parcialmente los vif,
# descartamos el cáliz inferior y construimos un nuevo modelo

fit2 <- lm(azucar ~ largo.l.tot + largo.tubo + caliz.sup + caliz.med + labio.sup + labio.inf, data = dat)
summary(fit2)
anova (fit2)

layout(matrix(1:4, 2, 2))
plot(fit2)
layout(1)

# revisando su colinealidad nuevamente
vif(fit2)

# Selección de modelos. Best subset selection.
# Construyendo todos los modelos posibles
# (pueden utilizarse forward o backward cambiando  el argumento method)
library(leaps)
BS.fit <- regsubsets(azucar ~ largo.l.tot + largo.tubo + caliz.sup + caliz.med + labio.sup + labio.inf, data = dat, method = "exhaustive")
BS.summary <- summary(BS.fit)
BS.summary

plot(BS.summary$cp, xlab = "número de variables", ylab = "Cp", type ="l")
plot(BS.summary$bic, xlab = "número de variables", ylab = "BIC", type ="l")

plot(BS.fit, scale = "Cp")
plot(BS.fit, scale = "bic")

coef(BS.fit, 3)
coef(BS.fit, 1)

# stepAIC (MASS) realiza una búsqueda automática del mejor modelo 
library(MASS) 
stepAIC(fit2, direction ="both") #para usar AIC

#para usar BIC cambio los grados de libertad de la penalización
n<-nrow(dat)
stepAIC(fit2, direction="both", k=log(n))#ajustar el modelo final

# Model averaging. Uso básico de MuMIn
library(arm)
library(MuMIn)

global <- lm(azucar ~ largo.l.tot + largo.tubo + caliz.sup + caliz.med + labio.sup + labio.inf, data = dat, na.action = na.fail)

std.model <- standardize(global, standardize.y = FALSE)

set <- dredge(std.model)
set

top.mod <-get.models(set, subset = delta<2)
top.mod

AVG<-model.avg(top.mod)
summary(AVG)

# Selección de Modelos. Shrinkage
# Lasso

library(glmnet)
x <- model.matrix(azucar ~ largo.l.tot + largo.tubo + caliz.sup + caliz.med + labio.sup + labio.inf, data = dat)[,-1]
y <- dat$azucar

lasso.mod <- glmnet(x, y, alpha = 1)
plot(lasso.mod)
coef(lasso.mod, s = 0.1) # ejemplo

# Cross validation.

cv.out <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
plot(cv.out)

cv.out$lambda.min
coef(cv.out, s = "lambda.min")

#### END ####