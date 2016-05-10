### Gráficos del paquete lattice

library (lattice)

fum <- read.table("C:/RD/fumadores.txt", header=T)
#  fum <- read.table(file=file.choose(), header=TRUE)

#gráficos bivariados
# Notar que ingresar los datos como una fórmula es más eficiente
xyplot(ca.pulm ~ alt, data = fum)
xyplot(ca.pulm ~ alt | edad, data = fum)
xyplot(ca.pulm ~ alt | fuma, data = fum)
xyplot(ca.pulm ~ alt | fuma*sexo, data = fum)
xyplot(ca.pulm ~ alt, groups = fuma, data = fum)

#ayuda de la funcion básica xyplot
?xyplot

#manipular alguna de sus opciones...
xyplot(ca.pulm ~ alt, groups = fuma, data = fum, xlab = "altura", 
       ylab = "capacidad\npulmonar", col = c("red", "black"), pch = 19, 
       lwd = 2, key = list(text = list(c("no fumadores", "fumadores")), 
                           space = "bottom", 
                           points = list(pch = 19, col = c("red", "black"))))

#GRÁFICOS DE CAJAS
bwplot(ca.pulm ~ fuma | sexo, data = fum)

#HISTOGRAMAS Y DIAGRAMAS DE DENSIDAD
histogram(~ ca.pulm, data = fum)
densityplot(~ ca.pulm | fuma, data = fum)

#############################################################

##Gráficos del paquete ggplot2

fum <- read.table("C:/RD/fumadores.txt", header=T)
#  fum <- read.table(file=file.choose(), header=TRUE)

library(ggplot2)

#utilizando ggplot
ggplot(data = fum, aes(x = edad, y = ca.pulm)) + geom_point()    

#notar como los gráficos se van acumulando
g1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm))  
g2 <- g1 + geom_point()
g2    

#modificando algunos detalles comunes
g1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = fuma)) 
g2 <- g1 + geom_point(size=3, aes(shape=sexo)) 
g3 <- g2 + xlab("Edad")  
g4 <- g3 + ylab("Capacidad Pulmonar")
g5 <- g4 + theme_bw()
g5

#modificando los colores 
g1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = fuma)) 
g2 <- g1 + scale_colour_manual(values=c("red", "black"))
g3 <- g2 + geom_point(size=3) 
g3  

#modificando los colores con una escala continua
g1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = alt)) 
g2 <- g1 + geom_point(size=3) 
g2  

#modificando tamaños con una escala continua
g1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, size = alt, color = fuma)) 
g2 <- g1 + geom_point() 
g2  

#Faceting (división del la ventana gráfica como en lattice)
g1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = fuma))
g2 <- g1 + geom_point(size=3) + theme_bw()
g3 <- g2 + facet_grid(. ~ sexo)
g3

g1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = fuma))
g2 <- g1 + geom_point(size = 3) + theme_bw()
g3 <- g2 + facet_grid(fuma ~ .) + scale_color_manual(values = c("red", "blue"))
g3

#agregar líneas de regresión
g1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = fuma))
g2 <- g1 + geom_point(size = 3) + theme_bw()
g3 <- g2 + geom_smooth(method = "lm")
g3

#gráficos de cajas
g1 <- ggplot(data = fum, aes(x = fuma, y = ca.pulm))
g2 <- g1 + geom_boxplot()
g2

#histogramas
g1 <- ggplot(data = fum, aes(x = ca.pulm))
g2 <- g1 + geom_histogram(fill="red")
g2

#### END ####
