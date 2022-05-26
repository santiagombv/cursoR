### Gráficos del paquete lattice

library(lattice)

# complete la ruta al directorio en ...
fum <- read.table(".../fumadores.txt", header=T)

#gráficos bivariados
# Notar que ingresar los datos como una fórmula es más eficiente
xyplot(ca.pulm ~ alt, data = fum)
xyplot(ca.pulm ~ alt | edad, data = fum)
xyplot(ca.pulm ~ alt | fuma, data = fum)
xyplot(ca.pulm ~ alt | fuma*sexo, data = fum)
xyplot(ca.pulm ~ alt, groups = fuma, data = fum)

# ayuda de la funcion básica xyplot
?xyplot

# manipular alguna de sus opciones...
xyplot(ca.pulm ~ alt, groups = fuma, data = fum, xlab = "altura", 
       ylab = "capacidad\npulmonar", col = c("red", "black"), pch = 19, 
       lwd = 2, key = list(text = list(c("no fumadores", "fumadores")), 
                           space = "bottom", 
                           points = list(pch = 19, col = c("red", "black"))))

# GRÁFICOS DE CAJAS
bwplot(ca.pulm ~ fuma | sexo, data = fum)

# HISTOGRAMAS Y DIAGRAMAS DE DENSIDAD
histogram(~ ca.pulm, data = fum)
densityplot(~ ca.pulm | fuma, data = fum)

#############################################################

## Gráficos del paquete ggplot2
library(ggplot2)
library(viridis)

# complete la ruta al directorio en ...
fum <- read.table(".../fumadores.txt", header=T)

#utilizando ggplot
ggplot(data = fum, aes(x = edad, y = ca.pulm)) + geom_point()    

# notar como los gráficos se van acumulando
g1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm))  
g2 <- g1 + geom_point()
g2    

# modificando algunos detalles comunes
# notar el + al final (equivale a una línea continua)
g1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = fuma)) +
  geom_point(size=3, aes(shape=sexo)) +
  xlab("Edad") +
  ylab("Capacidad Pulmonar") +
  theme_bw()
g1

# modificando los colores 
c1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = fuma)) +
  scale_colour_manual(values=c("red", "black")) + 
  geom_point(size=3) 
c1  

# modificando los colores con una escala continua
c2 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = alt)) +
  geom_point(size=3) 
c2  

# modificando los colores con una escala de viridis
c3 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = alt)) +
  geom_point(size=3) + scale_color_viridis_c(option = "magma")
c3  

# modificando tamaños con una escala continua
t1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, size = alt, color = fuma)) + 
  geom_point() 
t1

# Faceting (división del la ventana gráfica como en lattice)
f1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = fuma)) +
  geom_point(size=3) + theme_bw() + facet_grid(. ~ sexo)
f1

f2 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = fuma)) + 
  geom_point(size = 3) + theme_bw() + facet_grid(fuma ~ .) + 
  scale_color_manual(values = c("red", "blue"))
f2

f3 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = fuma)) + 
  geom_point(size = 3) + theme_bw() + 
  scale_color_manual(values = c("red", "blue")) +
  facet_wrap(~fuma*sexo, ncol = 2)
f3

# agregar líneas de regresión
m1 <- ggplot(data = fum, aes(x = edad, y = ca.pulm, color = fuma)) +
  geom_point(size = 3) + theme_bw() +
  geom_smooth(method = "lm")
m1

# gráficos de cajas
b1 <- ggplot(data = fum, aes(x = fuma, y = ca.pulm, fill = fuma)) + geom_boxplot() +
  scale_fill_viridis_d()
b1

# histogramas
h1 <- ggplot(data = fum, aes(x = ca.pulm)) + geom_histogram(fill="red")
h1

# combinando gráficos
library(patchwork)

c3 / b1

c3 + b1

c3 / (b1 + h1)

#### END ####
