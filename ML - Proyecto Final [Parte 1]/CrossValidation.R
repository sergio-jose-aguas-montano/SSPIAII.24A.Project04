library(ggplot2)
library(mlr)


# Convertir variables a factor
df.Ventas.Train$local <- as.factor(df.Ventas.Train$local)
df.Ventas.Test$local <- as.factor(df.Ventas.Test$local)

# Se utiliza el modelo de entrenamiento y se fija una variable objetivo (local)
tsk <- makeClassifTask(data = df.Ventas.Train, target = "local")
# Se hace una predicción mediante el modelo de testeo y la variable objetivo
tskpred <- makeClassifTask(data = df.Ventas.Test, target = "local")
# Definir esquema de validación cruzada
rdesc <- makeResampleDesc("CV", iters = 5)  # 5-fold cross-validation
# Lista creada por makeLearner
base <- c("classif.rpart", "classif.lda", "classif.svm")
# Nos devuelve una lista después de aplicar las funciones necesarias
lrns <- lapply(base, makeLearner)
lrns <- lapply(lrns, setPredictType, "prob")
# Se utiliza la predicción base y se utiliza las siguientes predicciones como características para un resultado
m <- makeStackedLearner(base.learners = lrns,
                        predict.type = "prob", method = "hill.climb")

# Entrenamiento con validación cruzada
resampling <- resample(m, tsk, rdesc, measures = list(acc, mmce, fpr, tpr))


#Segundo modelo
#Se vuelve a generar un modelo parecido al descrito anteriormente, pero cambiando de variable objetivo (Venta)
tsk = makeClassifTask(data = df.Ventas.Train, target = "Venta")
tskpred = makeClassifTask(data = df.Ventas.Test, target = "Venta")
base = c("classif.rpart", "classif.lda", "classif.svm")
lrns = lapply(base, makeLearner)
lrns = lapply(lrns, setPredictType, "prob")
m = makeStackedLearner(base.learners = lrns,
                       predict.type = "prob", method = "hill.climb")

resampling <- resample(m, tsk, rdesc, measures = list(acc, mmce, fpr, tpr))
