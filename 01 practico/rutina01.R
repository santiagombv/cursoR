################################################################################
# El símbolo # desactiva el espacio a su derecha.
# Es útil para poner aclaraciones en nuestras rutinas.

# El comando más básico: Asignar crea objetos.
# Puede usarse = en su reemplazo o -> para asignar el nombre al final.
# Para unificar el estilo de todas las rutinas sólo usaré <-
n <- 15  
n   # escribir el nombre de un objeto es "invocarlo"

# al usar de nuevo el mismo nombre el objeto anterior se pierde ("pisarlo")
n <- 46 + 12  				 
n

# los caracteres categóricos necesitan ser ingresados con comillas
di <- "A"
di

# Obtener ayudas, el símbolo ?
# si tenemos dudas sobre el funcionamiento de una función
? lm

# Si desconocemos el nombre de una función para realizar determinada
# tarea, puede realizarse una búsqueda con ?? (sólo en los paquetes intalados)
?? "linear models"

# crear vectores. La función concatenar c
vector <- c(1, 2, 3, 4)
vector <- c("a", "b", "c", "d")

################################################################################

# vectores de repetición. 
# esta función tiene dos argumentos, separados por una coma
# rep(lo_que_queremos_repetir, cuantas veces)
xx <- rep("A", 50) 
xx

# secuencias 
seq1 <- c(1:50)  # el símbolo : indica desde:hasta
seq1

seq2 <- seq(from = 0, to = 0.99, by = 0.01) # argumentos explícitos
seq2

# combinando vectores de a pares: cbind y rbind
az <- cbind(seq1, xx)
za <- rbind(c(1:25), rnorm(25))	

# comparar
az
za

# construyendo una matriz: matrix()
m <- c(1:20)
matriz1 <- matrix(m, 4, 5) # (vector a usar, filas, columnas) 
matriz1

################################################################################

## Preparación e ingreso de datos: la función read.table

# la función read.table
# opción 1 con ruta completa (notar orientación de las barras /)
# en este caso asumimos que se encuentra en el directorio RD
datos <- read.table("C:/RD/datos/peces1.txt", header=TRUE)

# opción 2 seleccionando el directorio en uso previamente desde "Archivo"
# o con setwd
setwd("C:/RD/") ## "C:/RD/" es un ejemplo...
datos <- read.table("peces1.txt", header=TRUE)

# opción 3 Abre una ventana de búsqueda 
# desventaja: requiere que el humano trabaje!
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
nrow(datos)     # número de filas
ncol(datos)     # número de columnas
names(datos)    # nombre de las columnas

################################################################################

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

################################################################################

## Subdivisión de conjuntos de datos

dat1 <- subset(datos, datos$grupo == "A")
dat1

dat2 <- subset(datos, datos$grupo == "A" & datos$trat == 2)
dat2

# Crear una lista de bases de datos
ldat <- split(datos, f = datos$grupo) 
ldat
ldat[["B"]] # indexacion d listas

################################################################################

## Modificación y creación de columnas.

# reemplazar (al usar el mismo nombre) una variable numérica por un factor
datos$trat
datos$trat <- as.factor(datos$trat) 
datos$trat    # notar la lista de niveles del factor

# crear una nueva columna en una base de datos ya existentes
# (utilizo un nombre nuevo)
datos$pob <- c(rep("pob1", 15), rep("pob2", 15))

datos$log.largo.a <- log(datos$largo.a)

head(datos)

# crear una nueva columna uniendo clasificadores
datos$clave <- paste(datos$pob, datos$trat, sep = ".")
head(datos)

# Usando factor para arreglos más complicados
datos$pais <- factor(datos$pob, levels = c("pob2", "pob1"), 
                     labels = c("Bolivia", "Chile"))
head(datos)

################################################################################

## Funciones estadísticas básicas y de resumen
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

################################################################################

## Indexación, subdivisión y modificación en tidyverse.
library(dplyr)
# Subdivisión o *filtrado*
dat1 <- filter(datos, grupo == "A")
dat2 <- filter(datos, grupo == "A", largo.b > 100)

# Selección por colunmas
dat3 <- select(datos, grupo, largo.a)

# select tiene funciones de ayuda. Por ejemplo, seleccionar todas las 
# columnas que comiencen con "larg". Ver ?select para más ejemplos.
dat4 <- select(datos, starts_with("larg"))

# Modificación de columnas
# puede crearse un nuevo set de datos como aquí o modificarse el original
dat5 <- mutate(datos,
               prop = largo.b / largo.a, # columna nueva
               log.largo = log(largo.a)) # columna nueva ...

################################################################################

## Haciendo más eficiente la programación: esto es una pipa

library(magrittr)
library(dplyr)

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

# Usando dplyr y pipa de magrittr
# (mismo resultado usando |>)
filter(datos, grupo == "A") %>%
  select(largo.a) %>%
  summarize(mean(largo.a)) -> M
M

####### END #######

