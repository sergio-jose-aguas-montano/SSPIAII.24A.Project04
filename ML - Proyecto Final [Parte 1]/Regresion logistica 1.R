source("Análisis descriptivo.R")
library(mlr)
library(caret)
library(ggplot2)

#Modelo regresion logistica

#3 tipos
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


# Calcular las predicciones
predicciones <- res$data$response
# Valores reales
valores <- df.Ventas.Test$PrecioUnidad

# Calcular la suma de los cuadrados totales (SCT)
sct <- sum((valores - mean(valores))^2)

# Calcular la suma de los cuadrados de los residuos (SCR)
scr <- sum((valores - predicciones)^2)

# Calcular el R cuadrado
r2 <- 1 - (scr / sct)

# Número de observaciones
n <- length(valores)

# Número de predictores (en este caso, solo tienes un modelo)
p <- 1

# Calcular el R cuadrado ajustado
r2Ajustado <- 1 - (1 - r2) * ((n - 1) / (n - p - 1))

# Mostrar los resultados
print(paste("R cuadrado:", r2))
print(paste("R cuadrado ajustado:", r2Ajustado))


summary(res$data$response)
