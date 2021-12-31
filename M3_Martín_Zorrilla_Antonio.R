#Ejercicio 1---------------------------------------------------------------------

Personas <- read.table("C:/Users/ACER/Desktop/mdatos.txt", header=TRUE, 
    sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# 1. --> c ; 2. --> a

numSummary(Personas[,"Edad", drop=FALSE], statistics=c("mean", "sd"), 
    quantiles=c(0,.25,.5,.75,1))

# 3. --> b ;  4. --> a ; 5. --> c

numSummary(Personas[,"Peso", drop=FALSE], statistics=c("quantiles"), 
           quantiles=c(.5,.8))

# 6. --> b ;  7. --> h ; 

numSummary(Personas[,"Altura", drop=FALSE], statistics=c("skewness", "kurtosis"),
           quantiles=c(.5,.8), type="2")

# 8. --> b

numSummary(Personas[,"Altura", drop=FALSE], groups=Personas$Sexo, 
           statistics=c("mean"), quantiles=c(.5,.8))

# 9.

.Table <- with(Personas, table(Sexo))
cat("\ncounts:\n")
print(.Table)
cat("\npercentages:\n")
print(round(100*.Table/sum(.Table), 2))
})

  #El porcentaje de hombres es 36.67, el porcentaje de mujeres es 63.33

with(Personas, pie(table(Sexo), labels=levels(Sexo), xlab="", ylab="", main="Gráfico de sectores para la variable sexo", col=rainbow_hcl(2)))

# 10.

with(Personas, Barplot(Sexo, xlab="Sexo", ylab="Frecuencia", main="Gráfico de barras para la variable sexo", col=c("red", "white")))

# 11.

with(Personas, Hist(Altura, scale="frequency", breaks=4, col="darkgray", 
          xlab="Altura (cm)", ylab="Frecuencia", 
          main="Histograma para la variable altura"))

# 12.

Boxplot( ~ Altura, data=Personas, id=list(method="y"))
Boxplot( ~ Edad, data=Personas, id=list(method="y"))
Boxplot( ~ Peso, data=Personas, id=list(method="y"))

#En la variable donde podemos observar un valor atípico es en el peso


#Ejercicio 2---------------------------------------------------------------------

# 1.

local({
  .x <- seq(3.419, 16.581, length.out=1000)  
  plotDistr(.x, dnorm(.x, mean=10, sd=2), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("Normal Distribution:  Mean=10, Standard deviation=2"))
})

# 2. --> c

# La respuesta es 0.38915, ya que es el valor más semejante a la imagen que podemos observar en la gráfica de densidad de la distribución normal 

# 3. --> d

pnorm(c(10), mean=10, sd=2, lower.tail=TRUE)

# 4. --> d

qnorm(c(0.5), mean=10, sd=2, lower.tail=TRUE)

# 5.

v.a.n. <- as.data.frame(matrix(rnorm(1*100, mean=10, sd=4), ncol=100))
rownames(v.a.n.) <- "sample"
colnames(v.a.n.) <- paste("obs", 1:100, sep="")
v.a.n. <- as.data.frame(matrix(rnorm(100*1, mean=10, sd=4), ncol=1))
rownames(v.a.n.) <- paste("sample", 1:100, sep="")
colnames(v.a.n.) <- "obs"
numSummary(v.a.n.[,"obs", drop=FALSE], statistics=c("quantiles"), 
           quantiles=c(.5))


#Ejercicio 3---------------------------------------------------------------------

# 1. --> a

.Table <- data.frame(Probability=dbinom(0:25, size=25, prob=0.2))
rownames(.Table) <- 0:25 
print(.Table)

# 2. --> a

pbinom(c(9), size=25, prob=0.2, lower.tail=TRUE)

# 3. --> b

pbinom(c(12), size=25, prob=0.2, lower.tail=FALSE)
