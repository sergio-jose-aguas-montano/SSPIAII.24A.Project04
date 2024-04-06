source("libr.preprocess.R")
options(scipen = 999)
set.seed(2002)

#Importación de datos de atención y venta al cliente de 3 meses
df.Ventas <-read.csv("Atencion a cliente audioferre.csv",
                     header = T,
                     stringsAsFactors = T)

#Convertimos los datos categóricos a numéricos para 
#uso posterior
df.Ventas$Producto <- as.numeric(df.Ventas$Producto)
df.Ventas$Categoria <- as.numeric(df.Ventas$Categoria)
df.Ventas$Dia <- as.numeric(df.Ventas$Dia)
df.Ventas$Personal <- as.numeric(df.Ventas$Personal)
df.Ventas$local <- as.numeric(df.Ventas$local)
df.Ventas$Venta <- as.numeric(df.Ventas$Venta)
df.Ventas$Horario <- as.numeric(df.Ventas$Horario)
df.Ventas$Fecha <- as.numeric(df.Ventas$Fecha)

