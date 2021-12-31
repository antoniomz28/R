
Tipo.de.Fármaco <- factor(c("A", "B", "B", "B", "A", "A"))
Colesterol <- c(119, 99, 102, 78, 78, 135)
Pulso <- c(59, 89, 107, 76, 91, 70)

pacientes <-- data.frame(Tipo.de.Fármaco, Colesterol, Pulso)

#Ejercicio 1---------------------------------------------------------------------

sqrt(pacientes[5,3])

#Ejercicio 2---------------------------------------------------------------------

pacientes$Riesgo.Infarto <- with(pacientes, (0.3 * Colesterol + 0.45 * Pulso) / 2)
#El paciente con mayor riesgo de infarto es el 3 con 39.375

#Ejercicio 3---------------------------------------------------------------------

numSummary(pacientes[,"Colesterol", drop=FALSE], statistics=c("mean"), 
           quantiles=c(0,.25,.5,.75,1))
#La media del colesterol es el 101.8333 

#Ejercicio 4---------------------------------------------------------------------

numSummary(pacientes[,"Pulso", drop=FALSE], statistics=c("quantiles"), 
           quantiles=c(.5))
#La mediana del pulso es 82.5

#Ejercicio 5---------------------------------------------------------------------

Boxplot( ~ Colesterol, data=pacientes, id=list(method="y"), ylab="", 
         main="Diagrama de cajas para la variable colesterol")
#No hay ningún valor atípico

#Ejercicio 6---------------------------------------------------------------------

FarmacoA <- subset(pacientes, subset=Tipo.de.Fármaco == "A", 
                   select=c(Colesterol,Pulso,Riesgo.Infarto))
numSummary(FarmacoA[,"Colesterol", drop=FALSE], statistics=c("sd"), 
           quantiles=c(0,.25,.5,.75,1))

FarmacoB <- subset(pacientes, subset=Tipo.de.Fármaco == "B", 
                   select=c(Colesterol,Pulso,Riesgo.Infarto))
numSummary(FarmacoB[,"Colesterol", drop=FALSE], statistics=c("sd"), 
           quantiles=c(0,.25,.5,.75,1))

#Comparamos la desviación típica y el fármaco A tiene una mayor desviación típica y por tanto mayor varianza, 864.333540203

#Ejercicio 7---------------------------------------------------------------------

RegModel.4 <- lm(Pulso~Colesterol, data=pacientes)
summary(RegModel.4)

# Pulso = 117.2971 - 0.3466 * Colesterol

#Ejercicio 8---------------------------------------------------------------------

cor(pacientes[,c("Colesterol","Pulso")], use="complete")

#El coeficiente de correlación es -0.4564519

#Ejercicio 9---------------------------------------------------------------------

#H0: mu0=85
#H1: mu!=85

with(pacientes, (t.test(Pulso, alternative='two.sided', mu=85, 
                        conf.level=.90)))
#No se rechaza la hipótesis nula, el p valor es mayor que el nievel de significación
#La media es 82

#Ejercicio 10---------------------------------------------------------------------

with(pacientes, tapply(Colesterol, Tipo.de.Fármaco,  var, na.rm=TRUE))
var.test(Colesterol ~ Tipo.de.Fármaco, alternative='two.sided', conf.level=.95,
         data=pacientes)

#Como p-value = 0.3303 las varianzas son iguales ya que no se rechaza la hipótesis nula

t.test(Colesterol~Tipo.de.Fármaco, alternative='two.sided', conf.level=.95, 
       var.equal=TRUE, data=pacientes)

#Como p-value = 0.3955 es mayor que el nivel de significación, se consideran iguales
#La varianza de medicamento A es 110.6667 y la del medicamento B es 93.

