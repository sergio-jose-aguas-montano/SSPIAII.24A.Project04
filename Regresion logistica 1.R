source("Análisis descriptivo.R")

#Modelo regresion logistica

#2 tipos
#Se utiliza el modelo de entrenamiento y se fija una variable objetivo (PrecioUnidad)
tsk = makeRegrTask(data = df.Ventas.Train, target = "PrecioUnidad")
#Tambien se utiliza el modelo de testeo
tskTest = makeRegrTask(data = df.Ventas.Test, target = "PrecioUnidad")
#lista creada por makeLearner
base = c("regr.rpart", "regr.svm","regr.randomForest")
#lapply nos devuelve una lista despues de aplicar una funcion a sus elementos
lrns = lapply(base, makeLearner)
#se utiliza la prediciion base y se utiliza las siguientes predicciones como acracteristicas para un resultado
m = makeStackedLearner(base.learners = lrns,
                       predict.type = "response", method = "compress")
tmp <- train(m, tsk)

res <- predict(tmp, tskTest)


#Grafica
plt.Ventas <- ggplot()+
  theme_light()+
  ggtitle("Regresion: Precio vs Dia")+
  xlab("Dia")+
  ylab("Precio")

#Grafica utilizando las variables de Fecha y Precio por unidad
plt.Ventas.g <- plt.Ventas+
  geom_point(aes(x = df.Ventas.Test$Fecha,
                 y = df.Ventas.Test$PrecioUnidad),
             colour = "blue")+
  geom_line(aes(x = df.Ventas.Test$Fecha,
                y = res$data$response),
           colour = "coral")
plt.Ventas.g






