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

# Función para graficar variables categóricas
plot_categorical <- function(data, variable, title) {
  ggplot(data, aes_string(x=variable)) +
    geom_bar(fill="steelblue") +
    theme_minimal() +
    labs(title=title, x=variable, y="Count")
}

# Variables categóricas
plot_categorical(df.Ventas, "Producto", "Cantidad de cada Producto")
plot_categorical(df.Ventas, "Categoria", "Cantidad de cada Categoria")
plot_categorical(df.Ventas, "Personal", "Cantidad de cada Personal")
plot_categorical(df.Ventas, "local", "Cantidad de cada Local")

# Función para graficar variables numéricas
plot_numerical <- function(data, variable, title) {
  ggplot(data, aes_string(x=variable)) +
    geom_histogram(fill="steelblue", bins=30) +
    theme_minimal() +
    labs(title=title, x=variable, y="Count")
}

# Variables numéricas
plot_numerical(df.Ventas, "Cantidad", "Distribución de Cantidad")
plot_numerical(df.Ventas, "PrecioUnidad", "Distribución de Precio por Unidad")
plot_numerical(df.Ventas, "PrecioFinal", "Distribución de Precio Final")
plot_numerical(df.Ventas, "Dinero.recibido", "Distribución de Dinero Recibido")
plot_numerical(df.Ventas, "Horario", "Distribución de Horario")
plot_numerical(df.Ventas, "Dia", "Distribución de Dia")
plot_numerical(df.Ventas, "Fecha", "Distribución de Fecha")
plot_numerical(df.Ventas, "Venta", "Distribución de Venta")
plot_numerical(df.Ventas, "local", "Residente o extranjero")

