source("Análisis descriptivo.R")

#Modelo de K-means
mdl.NSup <- kmeans(df.Ventas.Train, 7, trace = T)

#Elbow (Saber cantidad de clusters)
n.obs <- length(df.Ventas.Train$Categoria)

#Y - wcss
#X - Númeroclusters
wcss <- vector()
for (i in 1:15) {
  wcss[i] <- kmeans(df.Ventas.Train, i)$tot.withinss
}

wcss <- as.data.frame(wcss)
wcss$k <- seq(1,15,1) #Del 1 al 15, contando de 1 en 1

#Graficación del método del codo para ver la curvatura para saber el número de clusters necesarios
ggplot()+
  geom_line(aes(x = wcss$k,
                y = wcss$wcss))+
  geom_point(aes(x = wcss$k,
                 y = wcss$wcss,
                 color = wcss$k))+
  ggtitle("Método del codo")+
  xlab("Iteración")+
  ylab("WCSS")+
  theme_light()

#Uso de ggplot base para representar los cluster, básico solo mandar la información de x y y en el plano junto con el modelo para los puntos
ggplot(df.Ventas.Train, aes(x = PrecioUnidad, y = Producto)) +
  geom_point(aes(color = as.factor(mdl.NSup$cluster))) +
  labs(title = "Relacion de Producto y precio por unidad", x = "Precio por unidad", y = "Producto", color = "Cluster") +
  theme_minimal()