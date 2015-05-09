##Caso1

ulm <- read.table("C:/RD/ulmifolia.txt", header = TRUE)
#  ulm <- read.table(file = file.choose(), header = TRUE)

reg <- lm(semillas ~ flores + nectar + tamano, data = ulm)
summary(reg)

layout(matrix(1:4, 2, 2))
plot(reg)
layout(1)

# Guardamos los coeficientes observados
Bobs <- reg$coeff
Bobs

# BOOTSTRAP 
library(boot)

# PASO 1: crear una función para "bootstrapear"
REG <- function(datos, indices) {
  newdata <- datos[indices, ]  #sobre esta línea actuará la función boot  
  fit <- lm(semillas ~ flores + nectar + tamano, data = newdata)
  fit$coeff
}

# PASO 2: el bootstrap en sí
?boot

boot.reg <- boot(ulm, REG, 9999)
boot.reg
summary(boot.reg)

#histograma de los resultados
layout(matrix(1:4, 2, 2))
hist(boot.reg$t[, 1]) 
abline(v = Bobs[1], col="red") 
hist(boot.reg$t[, 2]) 
abline(v = Bobs[2], col="red") 
hist(boot.reg$t[, 3]) 
abline(v = Bobs[3], col = "red") 
hist(boot.reg$t[,4]) 
abline(v = Bobs[4], col = "red") 
layout(1)

## PASO 3: intervalos de confianza
?boot.ci #examinar la ayuda

# prueba, los 5 intervalos distintos que calcula R
int3.95<-boot.ci(boot.reg, conf = 0.90, type = "all", index = 3)
int3.95

# Examinamos que el intervalo de confianza NO incluya al 0
#(nos salteamos la significancia de la ordenada al origen)
# intervalos para la pendiente de "flores"
int2.90 <- boot.ci(boot.reg, conf = 0.90, type = "bca", index = 2)
int2.95 <- boot.ci(boot.reg, conf = 0.95, type = "bca", index = 2)
int2.99 <- boot.ci(boot.reg, conf = 0.99, type = "bca", index = 2)
int2.999 <- boot.ci(boot.reg, conf = 0.999, type = "bca", index = 2)

int2.90
int2.95
int2.99
int2.999

# intervalos para la pendiente de "nectar"
int3.90 <- boot.ci(boot.reg, conf = 0.90, type = "bca", index = 3)
int3.95 <- boot.ci(boot.reg, conf = 0.95, type = "bca", index = 3)
int3.99 <- boot.ci(boot.reg, conf = 0.99, type = "bca", index = 3)
int3.999 <- boot.ci(boot.reg, conf = 0.999, type = "bca", index = 3)

int3.90
int3.95
int3.99
int3.999

# intervalos para la pendiente de "tamaño"
int4.90 <- boot.ci(boot.reg, conf = 0.90, type = "bca", index = 4)
int4.95 <- boot.ci(boot.reg, conf = 0.95, type = "bca", index = 4)
int4.99 <- boot.ci(boot.reg, conf = 0.99, type = "bca", index = 4)
int4.999 <- boot.ci(boot.reg, conf = 0.999, type = "bca", index = 4)

int4.90
int4.95
int4.99
int4.999

# PASO 4: Un plot de diagnóstico: el jack.after.boot
# influencia de las observaciones individuales sobre la pendiente de 
# "flores"
jack.after.boot(boot.reg, index = 2) 

# ídem sobre la pendiente de "néctar"
jack.after.boot(boot.reg, index = 3) 


# ídem sobre la pendiente de "tamaño"
jack.after.boot(boot.reg, index = 4) 

# la línea horizontal es una medida de la influencia de cada observación
# la línea vertical presenta los percentiles 5, 10, 16, 50, 84, 90 y 95 de # la distribución del coeficiente

# Otro plot de diagnóstico alternativo puede realizarse con plot
# esto nos puede ayudar a decidir el tipo de intervalo

plot(boot.reg, index = 2, jack = T)
plot(boot.reg, index = 3, jack = T)
plot(boot.reg, index = 4, jack = T)

#### END ####