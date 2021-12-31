#Ejercicio 1---------------------------------------------------------------------

personas <- read.table("C:/Users/ACER/Desktop/personas.txt", header=TRUE, 
                       sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# .

cor(personas[,c("Altura","Peso")], use="complete")

#Entre las variables altura y peso existe una asociación moderada positiva

# .

cor(personas[,c("Altura","Peso", "Edad")], use="complete")


# .

#H0:p0=p
#H1:p0!=p

with(personas, cor.test(Altura, Edad, alternative="two.sided", method="pearson"))
# Como p-value = 0.7409 es mayor a 0.05 no rechazamos la hipótesis nula

scatterplotMatrix(~Altura+Edad, regLine=FALSE, smooth=FALSE, diagonal=list(method="density"), data=personas)

# .

RegModel.1 <- lm(Peso~Altura, data=personas)
summary(RegModel.1)

#Como p-value: 0.00001064, es menor que 0.05, por lo tanto los dos parámetros de la recta de regresión lineal son 0 y la regresión lineal no tiene sentido


