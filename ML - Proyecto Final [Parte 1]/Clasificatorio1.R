source("Análisis descriptivo.R")

#Clasificador 3 tipos
df.Ventas.Train$local<-as.factor(df.Ventas.Train$local)
df.Ventas.Test$local<-as.factor(df.Ventas.Test$local)

tsk = makeClassifTask(data = df.Ventas.Train, target = "local")
tskpred = makeClassifTask(data = df.Ventas.Test, target = "local")
base = c("classif.rpart", "classif.lda", "classif.svm")
lrns = lapply(base, makeLearner)
lrns = lapply(lrns, setPredictType, "prob")
m = makeStackedLearner(base.learners = lrns,
                       predict.type = "prob", method = "hill.climb")
#Entrenamiento
tmp = train(m, tsk)
#Prediccion
res = predict(tmp, tskpred)


calculateROCMeasures(res)

matriz <- table(df.Ventas.Test$Venta,res$data$response)
confusionMatrix(matriz,
                mode = "everything")

roc.classif = generateThreshVsPerfData(res, measures = list(fpr, tpr, mmce))
plotROCCurves(roc.classif)

#Clasificador 3 tipos
df.Ventas.Train$Venta<-as.factor(df.Ventas.Train$Venta)
df.Ventas.Test$Venta<-as.factor(df.Ventas.Test$Venta)

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

matriz <- table(df.Ventas.Test$Venta,res$data$response)
confusionMatrix(matriz,
                mode = "everything")

roc.classif = generateThreshVsPerfData(res, measures = list(fpr, tpr, mmce))
plotROCCurves(roc.classif)

