source("Análisis descriptivo.R")

#Modelo regresion logistica
mdl.Rlog <- glm(formula = Fecha ~ Producto,
                data = df.Ventas.Train)

summary(mdl.Rlog)

#Prediccion
predict.Ventas <- predict(mdl.Rlog, type = "response",
                          newdata = df.Ventas.Test)
predict.Ventas

Y.pred <- ifelse(predict.Ventas >= 0.5, 1, 0)
Y.pred

#Grafica
plt.Ventas <- ggplot()+
  theme_light()+
  ggtitle("Regresion: Fecha vs Producto")+
  xlab("Producto")+
  ylab("Fechas")

plt.Ventas.g <- plt.Ventas+
  geom_point(aes(x = df.Ventas.Train$Producto,
                 y = df.Ventas.Train$Fecha),
             colour = "blue")+
  geom_line(aes(x = df.Ventas$Producto,
                y = predict(mdl.Rlog,
                            newdata = df.Ventas)),
            colour = "coral")

plt.Ventas.g



