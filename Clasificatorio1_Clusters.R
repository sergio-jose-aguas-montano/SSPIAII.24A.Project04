source("Análisis descriptivo.R")

#Modelo de K-means
mdl.NSup <- kmeans(df.Ventas, 3, trace = T)

#Elbow (Saber cantidad de clusters)
n.obs <- length(df.Ventas$Categoria)

#Y - wcss
#X - N.clusters
wcss <- vector()
for (i in 1:15) {
  wcss[i] <- kmeans(df.Ventas, i)$tot.withinss
}

wcss <- as.data.frame(wcss)
wcss$k <- seq(1,15,1)

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
ggplot(datos, aes(x = Categoria, y = Venta)) +
  geom_point(aes(color = as.factor(mdl.NSup$cluster))) +
  labs(title = "Clusters con ggplot2", x = "Categoria", y = "Venta", color = "Cluster") +
  theme_minimal()