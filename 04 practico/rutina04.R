##Caso 1.
dat <- read.table("C:/RD/s_poly.txt", header = TRUE)
#  dat <- read.table(file = file.choose(), header = TRUE)

# exploración de los datos
layout(matrix(1:8,2,4))
plot(dat$azucar); plot(dat$largo.l.tot); plot(dat$largo.tubo) 
plot(dat$caliz.sup); plot(dat$caliz.med); plot(dat$caliz.inf) 
plot(dat$labio.sup); plot(dat$labio.inf)
layout(1)

which.max(dat$azucar)
dat <- dat[-101, ]

# exploración bivariada de los datos
pairs(dat)

###DETECCIÓN DE LA COLINEALIDAD
library(car)     		

#A) Gráficos SPLOM
scatterplotMatrix(~ largo.l.tot + largo.tubo + caliz.sup + caliz.med +
                    caliz.inf + labio.sup + labio.inf, #variables a graficar
                  reg.line=lm,            #añadir rectas de regresión a los gráficos
                  smooth=TRUE,            #añadir curvas suavizadas a los gráficos
                  diagonal = 'density',   #gráficos de la diagonal
                  data = dat)             #conjunto de datos 

#B) Matrices de correlación
CORR <- cor(dat[, c("largo.l.tot", "largo.tubo", "caliz.sup",
                    "caliz.med", "caliz.inf", "labio.sup", "labio.inf")], 
            use = "complete.obs") 
CORR

#C)Factores de inflación de la varianza
# primero construimos el modelo completo, sin interacciones

fit <- lm(azucar ~ largo.l.tot + largo.tubo + caliz.sup + caliz.med +
            caliz.inf + labio.sup + labio.inf, data = dat)
vif(fit)	

#Decisión: usar el criterio de r < 0.7 y parcialmente los vif,
#descartamos el cáliz inferior y construimos un nuevo modelo

fit2 <- lm(azucar ~ largo.l.tot + largo.tubo + caliz.sup + caliz.med + labio.sup + labio.inf, data = dat)
summary(fit2)
anova (fit2)
layout(matrix(1:4, 2, 2))
plot(fit2)
layout(1)

#revisando su colinealidad nuevamente
vif(fit2)

###SELECCIÓN DE MODELOS 
##anova puede usarse para comparar dos modelos anidados
fit2<-lm(azucar ~ largo.l.tot + largo.tubo + caliz.sup + caliz.med + labio.sup + labio.inf, data=dat)
fit3<-lm(azucar ~ largo.tubo + caliz.sup + caliz.med + labio.sup + labio.inf, data=dat) #sin largo.l.tot

anova(fit3, fit2)

##drop1 borra una variable del modelo a la vez y compara con el completo
drop1(fit2, test="F")

## stepAIC (MASS) realiza una búsqueda automática del mejor modelo 
library(MASS) 
stepAIC(fit2, direction ="both") #para usar AIC

#para usar BIC cambio los grados de libertad de la penalización
n<-nrow(dat)
stepAIC(fit2, direction="both", k=log(n))#ajustar el modelo final

### uso básico de MuMIn
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

#### END ####