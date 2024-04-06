#librerias
list.of.packages <- c("ggplot2","caTools","mltools","data.table","cowplot","e1071","caret","rpart","rpart.plot","randomForest","caret","cluster","factoextra","mlr","mlbench","ROCR","pROC")

new.packages <- 
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = T)


