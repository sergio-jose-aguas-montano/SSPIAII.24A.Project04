source("Preprocesamiento.R")

#correlación para ver que relación tienes las categorías
cor.ventas <- cor(df.Ventas)

#Exporacion de minimo, maximo, media
Summ.ventas <- summary(df.Ventas)

#Boxplot para ver los cuartiles y si hay datos desplazados
boxplot(df.Ventas$Producto)
boxplot(df.Ventas$Categoria)
boxplot(df.Ventas$Cantidad)
boxplot(df.Ventas$PrecioUnidad)
boxplot(df.Ventas$PrecioFinal)
boxplot(df.Ventas$Dinero.recibido)
boxplot(df.Ventas$Horario)
boxplot(df.Ventas$Dia)
boxplot(df.Ventas$Fecha)
boxplot(df.Ventas$Personal)
boxplot(df.Ventas$local)
boxplot(df.Ventas$Venta)


#División de los datos para el modelado y graficado
Split <- sample.split(df.Ventas$Venta, SplitRatio = 0.8)
df.Ventas.Train <- subset(df.Ventas, Split == T)
df.Ventas.Test <- subset(df.Ventas, Split == F)

#Graficacion para ver los datos dependiendo de 2 variables y una para la eleccion de variable por comportamiento
plot(df.Ventas)

with(df.Ventas.Train,plot(Fecha,Producto))

