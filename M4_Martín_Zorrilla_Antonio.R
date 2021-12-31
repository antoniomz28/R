#Ejercicio 1---------------------------------------------------------------------

mujeres <- read.table("C:/Users/ACER/Desktop/mujeres.txt", header=TRUE, 
                      sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# a)

# H0 --> ?? = ??0
# H1 --> ?? ??? ??0

v.a.n.alt = sample(x = mujeres$Altura, size = 10) #179 160 169 156 158 178 175 159 166 163
media = mean(v.a.n.alt)

z0 = (media - 170) / (7 / sqrt(18))
z0.025 = qnorm(p = 0.95, mean = 0, sd = 1)

#Como |z0| es mayor que z0.025 se rechaza la hipótesis nula, es decir 

# b)

v.a.n.peso = sample(x = mujeres$Peso, size = 8)
with(v.a.n.peso, (t.test(v.a.n.peso, alternative='greater', mu=55, conf.level=.90)))
#Como p-value = 0.03625 es menor que el nivel de significación (0.10) rechazamos la hipótesis nula, es decir, el peso de las mujeres es mayor que 55

# c)

with(mujeres, tapply(Altura, Raza,  var, na.rm=TRUE))
var.test(Altura ~ Raza, alternative='two.sided', conf.level=.95, 
         data=mujeres)
#Como p-value = 0.4131 es mayor que 0.05 no rechazamos la hipótesis nula y asumimos que las varianzas iguales


#Ejercicio 2---------------------------------------------------------------------

tension <- read.table("C:/Users/ACER/Desktop/tension.txt", header=TRUE, 
                      sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# a)

local({
  .Table <- xtabs(~ Tension , data= tension )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='two.sided', p=.2, conf.level=.90, 
            correct=FALSE)
})

#Como p-value = 0.1967 es mayor que 0.1 no rechazamos la hipótesis nula y asumimos que el porcentaje 

# b)

tension.alta <- subset(tension, subset=Tension == "alta", select=c(Sexo))
local({
  .Table <- xtabs(~ Sexo , data= tension.alta )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='greater', p=.5, conf.level=.95, correct=FALSE)
})
#La proporción de hombres con tensión alta, se rechaza la hipótesis nula