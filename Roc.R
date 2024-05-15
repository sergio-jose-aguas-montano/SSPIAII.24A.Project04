source("Análisis descriptivo.R")
install.packages("ROCR")
install.packages("pROC")
library(pROC)
library(ROCR)
library(gplots)

denprod<-density(df.Ventas.Train$PrecioUnidad)
denloc<-density(df.Ventas.Train$local)


pred <- prediction(df.Ventas.Train$Venta, df.Ventas.Train$local)
perf <- performance(pred,measure="tpr",x.measure="fpr")

plot(perf, colorize=TRUE)

abline(a=0,b=1)

Auc <- performance(pred,measure="auc")
Aucaltura <- Auc@y.values

cost.perf <- performance(pred, measure = "cost")
opt.cut <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

x <-perf@x.values[[1]][which.min(cost.perf@y.values[[1]])]
y <-perf@y.values[[1]][which.min(cost.perf@y.values[[1]])]

points(x,y, pch=20,col="red")
cat("Auc:", Aucaltura[[1]])
cat("Punto de corte optimo",opt.cut)

