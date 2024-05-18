# Carga de librerias
# ggplot y mlr se cargan para visualización y 
# aprendizaje automático
library(ggplot2)
library(mlr)


# Convertir variables a factor:
# convertimos las variables "local" y "Venta" a factor.
df.Ventas.Train$local <- as.factor(df.Ventas.Train$local)
df.Ventas.Test$local <- as.factor(df.Ventas.Test$local)

# Se utiliza el modelo de entrenamiento y se fija una variable objetivo (local)
tsk <- makeClassifTask(data = df.Ventas.Train, target = "local")

# Creamos una tarea de prediccion con "tskpred", especificando
# el conjunto de datos de prueba "df.Ventas.Test" y la variable
# objetivo "local"
tskpred <- makeClassifTask(data = df.Ventas.Test, target = "local")

# Se define el esquema de validacion cruzada "rdesc" con 
# 5-fold cross-validation, lo que significa que los datos 
# se dividen en 5 partes y el modelo se entrena y evalua 5 veces
# utilizando una parte diferente como conjunto de validación en cada iteración
rdesc <- makeResampleDesc("CV", iters = 5)

# Lista creada por makeLearner usando arboles ded desicion "classif.rpart", 
# análisis discriminante lineal (classif.lda), y 
#máquinas de vectores de soporte (classif.svm)
base <- c("classif.rpart", "classif.lda", "classif.svm")

# Nos devuelve una lista después de aplicar las funciones necesarias
lrns <- lapply(base, makeLearner)
lrns <- lapply(lrns, setPredictType, "prob")

# Se crea un modelo apilado (m) que combina los modelos base usando 
# el método de hill climb (method = "hill.climb") para determinar 
# la mejor combinación de modelos.
m <- makeStackedLearner(base.learners = lrns,
                        predict.type = "prob", method = "hill.climb")

# Entrenamiento con validación cruzada:
# Se entrena y evalua el módelo usando el resultado de "rdesc".
# Las medidas de evaluacion usadas son: acc (presicion), mmce(tasa derror de clasificacion),
# fpr(tasa de falsos positivos) y tpr(tasa de verdaderos positivos)
resampling <- resample(m, tsk, rdesc, measures = list(acc, mmce, fpr, tpr))


#Segundo modelo
#Se vuelve a generar un modelo parecido al descrito anteriormente, 
#pero cambiando de variable objetivo (Venta)
tsk = makeClassifTask(data = df.Ventas.Train, target = "Venta")
tskpred = makeClassifTask(data = df.Ventas.Test, target = "Venta")
base = c("classif.rpart", "classif.lda", "classif.svm")
lrns = lapply(base, makeLearner)
lrns = lapply(lrns, setPredictType, "prob")
m = makeStackedLearner(base.learners = lrns,
                       predict.type = "prob", method = "hill.climb")

#Realiza el remuestreo del model y calcula métricas de rendimiento
resampling <- resample(m, tsk, rdesc, measures = list(acc, mmce, fpr, tpr))
