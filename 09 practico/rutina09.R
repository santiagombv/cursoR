##Caso 1. Modelos lineales randomizados.

vacas <- read.table("C:/RD/vacas.txt", header = TRUE)
#  vacas <- read.table(file = file.choose(), header = TRUE)

fit <- lm(fat ~ breed, data = vacas)
summary(fit)
anova(fit)
layout(matrix(1:4, 2, 2))
plot(fit)  #examinar violación de supuestos
layout(1)

#Guardar las F observadas
Fobs <- anova(fit)[1, 4]
Fobs

#construir una function que repita los pasos del modelo
rd.aov <- function(Y, X){
  s.Y <- sample(x = Y, size = length(Y), replace = F)   #"reshuffling"
  fit <- lm(s.Y ~ X)                   #modelo lineal
  res <- anova(fit)[1, 4]              #extracción de las F
  c(res)                               #mostrar: un vector de F
}  

#Probar la función varias veces (debe dar distinto resultado)
rd.aov(Y = vacas$fat, X = vacas$breed)
rd.aov(Y = vacas$fat, X = vacas$breed)
#Realizar réplicas para obtener la distribución de pseudo F
pseudoF <- replicate(1000, rd.aov(Y = vacas$fat, X = vacas$breed))
PF <- t(pseudoF)	#transposición para que cada F quede en una columna

#histograma para ver los resultados simulados y los observados
hist(PF)
abline(v = Fobs, col = "red")

hist(PF, xlim=c(0, 51))
abline(v = Fobs, col = "red")

#valores P
P.breed <- length(PF[PF >= Fobs]) / length(PF)
P.breed  
#como nuestra simulación llegó hasta mil 
#no podemos asegurar que sea 0, sino P<0.001

#############################################################

##Caso 2: Un Monte Carlo sencillo.

esp <- read.table("C:/RD/espMC.txt", header = TRUE)
#  esp <- read.table(file = file.choose(), header = TRUE)
plot(y ~ x, data=esp)

#calcular las distancias euclídeas entre las filas de la base de datos
#como parámetro observado se elige la media  de todas las distancias 

Dobs<-mean(dist(esp))		#revisar la ayuda de la función dist
Dobs

#Realizamos una simulación de la distribución de los árboles según una #distribución uniforme
X.azar <- runif(n = nrow(esp), min = 0, max = 2)		
Y.azar <- runif(n = nrow(esp), min = 0, max = 2)
plot(X.azar, Y.azar)
Dazar <- mean(dist(cbind(X.azar, Y.azar)))
Dazar

#Repetimos la simulación 1000 veces y guardamos los resultados
Ds <- numeric(length(1000))
for (i in 1:1000) {
  X.azar <- runif(n=24, min=0, max=2)		
  Y.azar <- runif(n=24, min=0, max=2)
  Ds[i] <- mean(dist(cbind(X.azar, Y.azar)))
}

#visualizamos los resultados con un histograma
hist(Ds)
abline(v = Dobs, col = "red")
#calcular el valor P
P <- length(Ds[Ds < Dobs]) / length(Ds)
P

#### END ####