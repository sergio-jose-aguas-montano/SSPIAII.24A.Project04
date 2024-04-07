source("Análisis descriptivo.R")

#Clasificador 3 tipos
df.Ventas.Train$local<-as.factor(df.Ventas.Train$local)
df.Ventas.Test$local<-as.factor(df.Ventas.Test$local)

#Se utiliza el modelo de entrenamiento y se fija una variable objetivo (local)
tsk = makeClassifTask(data = df.Ventas.Train, target = "local")
#Se hace una prediccion mediante el modelo de testeo y la variable objetivo
tskpred = makeClassifTask(data = df.Ventas.Test, target = "local")
#lista creada por makeLearner
base = c("classif.rpart", "classif.lda", "classif.svm")
#Nos devuelve una lista despues de aplicar las funciones necesarias
lrns = lapply(base, makeLearner)
lrns = lapply(lrns, setPredictType, "prob")
#se utiliza la predicion base y se utiliza las siguientes predicciones como caracteristicas para un resultado
m = makeStackedLearner(base.learners = lrns,
                       predict.type = "prob", method = "hill.climb")
#Entrenamiento
tmp = train(m, tsk)
#Prediccion
res = predict(tmp, tskpred)


calculateROCMeasures(res)

#Matriz de confusion
matriz <- table(df.Ventas.Test$Venta,res$data$response)
confusionMatrix(matriz,
                mode = "everything")

#evaluacion y desempeño del clasificador
roc.classif = generateThreshVsPerfData(res, measures = list(fpr, tpr, mmce))
plotROCCurves(roc.classif)

#Clasificador 3 tipos
df.Ventas.Train$Venta<-as.factor(df.Ventas.Train$Venta)
df.Ventas.Test$Venta<-as.factor(df.Ventas.Test$Venta)

#Se vuelve a generar un modelo parecido al descrito anteriormente, pero cambiando de variable objetivo (Venta)
tsk = makeClassifTask(data = df.Ventas.Train, target = "Venta")
tskpred = makeClassifTask(data = df.Ventas.Test, target = "Venta")
base = c("classif.rpart", "classif.lda", "classif.svm")
lrns = lapply(base, makeLearner)
lrns = lapply(lrns, setPredictType, "prob")
m = makeStackedLearner(base.learners = lrns,
                       predict.type = "prob", method = "hill.climb")
#Entrenamietno
tmp = train(m, tsk)
#Prediccion
res = predict(tmp, tskpred)

#Matriz de confusion
matriz <- table(df.Ventas.Test$Venta,res$data$response)
confusionMatrix(matriz,
                mode = "everything")

#evaluacion y desempeño del clasificador
roc.classif = generateThreshVsPerfData(res, measures = list(fpr, tpr, mmce))
plotROCCurves(roc.classif)

