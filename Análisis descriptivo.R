source("Preprocesamiento.R")

#Correlacion para ver que relacion tienes las categorias
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

#Escalado de los datos por el sego que tienen en la boxplot
#df.Ventas$Cantidad <- scale(df.Ventas$Cantidad)
#df.Ventas$PrecioUnidad <- scale(df.Ventas$PrecioUnidad)
#df.Ventas$PrecioFinal <- scale(df.Ventas$PrecioFinal)
#df.Ventas$Dinero.recibido <- scale(df.Ventas$Dinero.recibido)

#df.Ventas$Cantidad<- as.numeric(df.Ventas$Cantidad)
#df.Ventas$PrecioUnidad<- as.numeric(df.Ventas$PrecioUnidad)
#df.Ventas$PrecioFinal<- as.numeric(df.Ventas$PrecioFinal)
#df.Ventas$Dinero.recibido<- as.numeric(df.Ventas$Dinero.recibido)


#Divicion de los datos para el modelado y graficado
Split <- sample.split(df.Ventas$Venta, SplitRatio = 0.8)
df.Ventas.Train <- subset(df.Ventas, Split == T)
df.Ventas.Test <- subset(df.Ventas, Split == F)

#Graficacion para ver los datos dependiendo de 2 variables
plot(df.Ventas.Train$Fecha)
with(df.Ventas.Train,plot(Fecha,Producto))
