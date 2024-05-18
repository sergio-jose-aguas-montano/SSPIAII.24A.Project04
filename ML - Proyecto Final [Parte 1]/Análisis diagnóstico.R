source("Análisis descriptivo.R")

#Modelo lineal generalizado de precio y dia 
plot(df.Ventas.Train$PrecioFinal)

mdl.Rlog <- glm(formula = PrecioFinal ~ Dia,
                data = df.Ventas.Train)
#Dia = variable independiente
#Precio Final = variable dependiente


#Resumen del modelo
summary(mdl.Rlog)

#Prediccion a partir del modelo mdl.Rlog
#Type = "response" indica que queremos que las probabilidades varien
#entre 0 y 1 
predict.Ventas <- predict(mdl.Rlog, type = "response",
                          newdata = df.Ventas.Test)

#Mostramos las predicciones
predict.Ventas

#Se hace la clasificacion de predicciones en dos categorias
#Si es mayor o igual a 0.5 pertenece a la clase positiva 1, 
#sino pertenece a la negativa 0
Y.pred <- ifelse(predict.Ventas >= 0.5, 1, 0)
Y.pred

#Generamos la gráfica
#Establecemos la estructura de la grafica:
#con ggtitle establecemos el encabezado de la gráfica
#xlab indica los valores de la variable independiente
#ylab indica los valores de la variable dependiente
plt.Ventas <- ggplot()+
  theme_light()+
  ggtitle("Regresion: Precio vs Dia")+
  xlab("Dia")+
  ylab("Precio")

#Usando el data frame de Ventas.Test colocamos puntos usando 
#la variable independiente y la dependiente para ver la 
#distribucion de los precios finales en funcion de los dias.
plt.Ventas.g <- plt.Ventas+
  geom_point(aes(x = df.Ventas.Test$Dia,
                 y = df.Ventas.Test$PrecioFinal),
             colour = "blue")+
  geom_line(aes(x = df.Ventas.Train$Dia,
                y = predict(mdl.Rlog,
                            newdata = df.Ventas.Train)),
            colour = "coral")
#Añadimos una linea haciendo uso de los datos de entrenamiento
#solo que en esta ocación usaremos los datos predichos por mdl.Rlog

plt.Ventas.g


#Modelo KNN
#Hacemos la clasificacion mediante el algoritmo, donde train especifica
#los datos de entrenamiento, test los datos de prueba, cl conjunto de datos 
#para predecir y k es el número de vecinos más cercanos
knn.Ventas <-knn(train = df.Ventas.Train, test=df.Ventas.Test, cl = df.Ventas.Train$Venta, k=3, prob = T)

#Hacemos un resumen de las predicciones
summary(knn.Ventas)

#Guardamos los datos en un data frame
knn.Predictions <- as.data.frame(knn.Ventas)

#Creamos un gráfico para visualizar la clasificacion del módelo KNN
#donde tomamos las columnas de PrecioUnidad y Categoria, col asigna colores a 
#los puntos predichos, ldw asigna el grosor de los puntos predichos, main
#establece un titulo al graficoy xlab y ylab establece los nombres a los ejes
plot(df.Ventas.Test[, c("PrecioUnidad", "Categoria")], col = knn.Ventas ,lwd = 5, main = "KNN Classification", xlab = "Categoria", ylab = "Venta")

#En el gráfico añadimos puntos que se sobrepondran a los datos anteriores,
#donde tomamos las mismas columnas del mismo data frame que la anteriror 
#función, pch es un punto especifico de marcador, ldw es el grosor del punto y 
#col va a poner color a puntos de los datos del data frame de prueba
points(df.Ventas.Test[, c("PrecioUnidad", "Categoria")], pch = 20,lwd = 1, col = as.factor(df.Ventas.Test$Categoria))

#Ponemos una leyenda en la parte superior derecha usando los niveles de 
#predicción del módelo
legend("topright", legend = levels(knn.Ventas), col = 1:length(knn.Ventas), pch = 20)

#Evaluamos el rendimiento del módelo por medio de una matriz de confusión
confusionMatrix(as.factor(knn.Ventas),
                as.factor(df.Ventas.Test$Venta),
                mode = "everything")

#Calculamos el error de clasificación de predicciones incorrectas
#y la presición con 1 - misClassError e imprimimos la precisión
misClassError <- mean(knn.Predictions != df.Ventas.Test$Venta) 
print(paste('Accuracy =', 1-misClassError))

#Roc con local
#Evaluamos el performance de los módelos de clasificación por medio de curvas
#(Receiver Operating Characteristic)

#Tomamos en cuenta los datos de local y venta del data frame de entrenamiento 
#para hacer la predicción
pred <- prediction(df.Ventas.Train$local, df.Ventas.Train$Venta)

#Calculamos el rendimiento a partir de pred que son los datos de la predicción
perf <- performance(pred,measure="tpr",x.measure="fpr")

#Generamos el gráfico para ver la curva ROC, clorize es un gradiente para poder
#ver mejor como cambian TPR y FPR con diferentes umbrales
plot(perf, colorize=TRUE)

#abline dibuja una linea desde el origen hasta el punto (1,1)
abline(a=0,b=1)

#Calculamos el área bajo la curva y guardamos su valor en Aucaltura
Auc <- performance(pred,measure="auc")
Aucaltura <- Auc@y.values

#Calculamos el costo asociado a diferentes puntos de corte para encontrar el
#punto que minimiza más el costo
cost.perf <- performance(pred, measure = "cost")
opt.cut <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

#Imprimimos el valor de AUC y el punto de coste óptimo
cat("Auc:", Aucaltura[[1]])
cat("Punto de corte optimo",opt.cut)


#Modelo de K-means
#Guardamos en mdl.NSup los datos de la función kmeans tomando los datos de 
#entrenamiento y estableciendo un número de clusters (7) 
mdl.NSup <- kmeans(df.Ventas.Train, 7, trace = T)

#Elbow (Saber cantidad de clusters)
n.obs <- length(df.Ventas.Train$Categoria)

#Y - wcss
#X - Númeroclusters
#Aplicamos K-means al conjunto de datos con un número de cluster que varia 
#entre 1 y 15
wcss <- vector()
for (i in 1:15) {
  wcss[i] <- kmeans(df.Ventas.Train, i)$tot.withinss
}

#Establecemos wcss como un data frame y añadimos la columna k que guardara
#el número de clusters
wcss <- as.data.frame(wcss)
wcss$k <- seq(1,15,1) #Del 1 al 15, contando de 1 en 1

#Graficación del método del codo para ver la curvatura para saber el número 
#de clusters necesarios
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

#Uso de ggplot base para representar los cluster, básico solo mandar la 
#información de x y y en el plano junto con el modelo para los puntos
ggplot(df.Ventas.Train, aes(x = PrecioUnidad, y = Producto)) +
  geom_point(aes(color = as.factor(mdl.NSup$cluster))) +
  labs(title = "Relacion de Producto y precio por unidad", x = "Precio por unidad", y = "Producto", color = "Cluster") +
  theme_minimal()

