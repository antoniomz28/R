#Ejercicio 1----------------------------------------------------------------------------

# a)

distancia <- c(350, 596, 129, 330, 674, 210, 390, 455, 195, 547)
tiempo <- c(12, 31, 14, 21, 25, 16, 18, 22, 19, 39)

drones <- data.frame(distancia, tiempo)

# b
drones$distancia_km <- with(drones, distancia / 1000)


# c)
drones$tiempo_h <- with(drones, tiempo / 60)


# d)
drones$vel <- with(drones, distancia_km / tiempo_h)

#El dron más veloz es el 6 	y el dron menos veloz es el 3

# e)
drones <- within(drones, {
  tipo_drones <- Recode(vel, '0:0.99 = "Lento"; 1:hi = "Rapido"', as.factor=TRUE)
})

# f)
drones_rapidos <- subset(drones, subset=tipo_drones == "Rapido")

# g)
drones_rapidos$drones_rapidos <- with(drones_rapidos, sqrt(vel)+ 3 * distancia_km + 2.5)
#El dron más contaminante es el 5

#Ejercicio 2----------------------------------------------------------------------------

# a)
personas <- read.table("C:/Users/ACER/Desktop/grupo1.txt", header=TRUE, sep="\t",
                       na.strings="NA", dec=".", strip.white=TRUE)
# b)
personas <- within(personas, {
  Edad_rec <- Recode(Edad, 'lo:29 = "Joven"; 30:hi = "Adulto"', as.factor=TRUE)
})

# c)
personas$IMC <- with(personas, Peso / Altura ^ 2)

# d)
personas2 <- read.table("C:/Users/ACER/Desktop/grupo2.txt", header=TRUE, 
                        sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

personas_global <- mergeRows(personas, personas2, common.only=TRUE)

#e)
personas_bajas <- subset(personas_global, subset=Altura < "170")

#f)
personas_bajas$Edad_meses <- with(personas_bajas, Edad * 12)


