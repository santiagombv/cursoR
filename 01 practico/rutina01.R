#############################################################
# El símbolo # desactiva el espacio a su derecha.
# Es útil para poner aclaraciones en nuestras rutinas.

# El comando más básico: Asignar crea objetos.
n <- 15  
n   #escribir el nombre de un objeto es "invocarlo"

# al usar de nuevo el mismo nombre el objeto anterior se pierde ("pisar")
n <- 46 + 12  				 
n

# los caracteres categóricos necesitan ser ingresados con comillas
di <- "A"
di

# Obtener ayudas, el símbolo ?
# si tenemos dudas sobre el funcionamiento de una función
? mean
? lm

# Si desconocemos el nombre de una función para realizar determinada
# tarea, puede realizarse una búsqueda con ?? (en los paquetes activos)
?? "linear models"

# crear vectores. La función concatenar c
vector <- c(1, 2, 3, 4)
vector <- c("a", "b", "c", "d")

#############################################################

##Funciones básicas: creación de vectores, secuencias y matrices

# Ya vimos c concatenar
vec <- c(1, 4, 6, 3, 7)
vec

# vectores de repetición. rep(lo_que_queremos_repetir, cuantas veces)
# esta función tiene dos argumentos, separados por una coma
xx <- rep("A", 50) 
xx

# secuencias seq()
seq1 <- c(1:50)  # el símbolo : indica desde:hasta
seq1

seq2 <- seq(from = 0, to = 0.99, by = 0.01) # argumentos explícitos
seq2

# combinando vectores de a pares: cbind y rbind
az <- cbind(seq1, xx)
az

za <- rbind(c(1:25), rnorm(25))	
za

# construyendo una matriz: matrix()
m <- c(1:20)
matriz1 <- matrix(m, 4, 5) # (vector a usar, filas, columnas) 
matriz1

#############################################################

##Preparación e ingreso de datos: la función read.table

# la función read.table
# opción 1 con ruta completa (notar orientación de las barras /)
# en este caso asumimos que se encuentra en el directorio RD
datos <- read.table("C:/RD/datos/peces1.txt", header=TRUE)

# opción 2 seleccionando el directorio en uso previamente desde "Archivo"
# o con setwd
setwd("C:/RD/")
datos <- read.table("peces1.txt", header=TRUE)

# opción 3 Abre una ventana de búsqueda 
# desventaja: requiere que el humano piense!
datos <- read.table(file.choose(), header=TRUE)

# opción 4 la función read.csv
datos <- read.csv("C:/RD/peces1.csv", header=TRUE)

# los datos ya están guardados, pero si queremos verlos...
datos    # no siempre es buena idea, sobre todo si son muchos

# una forma práctica de ver solo las primeras filas es con head 
head(datos)

# o echar un vistazo a la estructura de los datos
# str informa sobre la estructura de cualquier objeto
str(datos)

# Información básica del set de datos
nrow(datos)     #Número de filas
ncol(datos)     #Número de columnas
names(datos)    #Nombre de las columnas

#############################################################

## Indexación

# Para seleccionar una columna del marco de datos utilizamos $
datos$grupo

# los corchetes indican el contenido de un conjunto de datos, 
# matriz o vector. La coma separa filas de columnas

#columnas
datos[, 1]                           
datos[, "grupo"]

# grupos de columnas
datos[, 1:3]                         
datos[, c("grupo", "largo.a")]

# filas y datos individuales
datos[2, ]                           
datos[2, 3]
datos[-2, 2]

# indexando por una condición
datos[datos$largo.a > 15, ]
datos[datos$largo.a > 15 & datos$grupo == "A", ]

# Indexación de vectores
vec <- datos$largo.a                 
vec[1]
vec[vec > 15]

#############################################################

## Modificación y creación de columnas.

# reemplazo (al usar el mismo nombre) una variable numérica por un factor
datos$trat
datos$trat <- as.factor(datos$trat) 
datos$trat    # notar la lista de niveles del factor

# crear una nueva columna en una base de datos ya existentes
datos$pob <- c(rep("pob1", 15), rep("pob2", 15))
datos$pob <- as.factor(datos$pob) 
datos$log.largo.a <- log(datos$largo.a)
head(datos)

# crear una nueva columna uniendo clasificadores
datos$clave <- paste(datos$pob, datos$trat, sep = ".")
head(datos)

# Usando factor para arreglos más complicados
datos$pais <- factor(datos$pob, levels = c("pob2", "pob1"), 
                     labels = c("Bolivia", "Chile"))
head(datos)

############################################################

## Subdivisión de conjuntos de datos

# Símbolos lógicos: 
# == (igual)
# != (distinto), 
# > (mayor), 
# < (menor)
# >= (mayor o igual), 
# <= (menor o igual).

dat1 <- subset(datos, datos$grupo == "A")
dat1

dat2 <- subset(datos, datos$grupo == "A" & datos$trat == 2)
dat2

# Crear una lista de bases de datos
ldat <- split(datos, f = datos$grupo) 
ldat
ldat[["B"]] # indexacion d listas

###################################################################

## Funciones para extraer información de una columna o de un vector

length(seq1)
max(datos$largo.a)
which.max(datos$largo.a)
min(datos$largo.a)
which.min(datos$largo.a)

###################################################################
##Funciones estadísticas básicas

# media
M <- mean(datos$largo.a, na.rm = TRUE)
M

# desvío estándar
S <- sd(datos$largo.a, na.rm = TRUE)
S

# varianza
V <- var(datos$largo.a, na.rm = TRUE)
V

# sobre un conjunto de datos, se obtiene una matriz de varianza-covarianza
VC <- var(datos[,c(3,4)], na.rm = TRUE)
VC

# de forma parecida, puede obtenerse una matriz de correlación
CO <- cor(datos[, c(3, 4)], use="complete.obs")  #complete.obs elimina NA
CO 

# en cambio, para realizar un test de correlación
cor.test(datos$largo.a, datos$largo.b, method = "pearson")

# resumen de datos
summary(datos)

# una función útil para aplicar una función a un subconjunto de los datos 
# es aggregate, con una estructura ligeramente más complicada

MG<-aggregate(datos$largo.a, by=list(datos$grupo), FUN=mean, na.rm=TRUE)
MG

###########################################################
## Haciendo más eficiente la programación: esto es una pipa

library(magrittr)

# Usando R base, con creación de objetos intermedios
datos <- read.table("peces1.txt", header=TRUE)
largo.a_A <- subset(datos$largo.a, datos$grupo == "A")
M <- mean(largo.a_A)
M

# Usando R base, evitando los objetos intermedios
# Notar cómo se debe leer de "adentro" hacia "afuera".
M <- mean(subset(datos$largo.a, datos$grupo == "A"))
M

# Usando la pipa de magrittr %>%
# Notar como sigue más de cerca la lógica de 
# un "lenguaje natural"
datos$largo.a %>%
  subset(datos$grupo == "A") %>%
  mean() -> M  # en magrittr los paréntesis que acompañan mean son opcionales
M

# Usando 'pipas nativas' |>
datos$largo.a |>
  subset(datos$grupo == "A") |>
  mean() -> M
M

<<<<<<< HEAD
####### END #######
=======

####### END #######

>>>>>>> master
