#Ejercicio 1---------------------------------------------------------------------

# a)

p1 = c(7.5,12,14.5)
p2 = c(12.5,10.5,13,9,18.5)
p3 = c(11,8,7.5,9.5,19,14)
p4 = c(12.5,16,9.5,10)

# b)

p = list(p1,p4,p2,p3)

# c)

tam = 3

for (i in p){
  
  ifelse(i[1] <= i[tam],print("Si lo supera"),print("No lo supera"))
 
  tam = tam + 1
}
  

#Ejercicio 2---------------------------------------------------------------------



x = c(2,5,3,6,4,7,4,6,3,6,3,6,3,9,8,6)
suma = 0

for (i in 1:16){
  if (i %% 2==0){
  suma = suma + x[i]
  }
}
print(suma)




#Ejercicio 3--------------------------------------------------------------------



n = 6
factorial = 1

for (i in 1:n){
  
  factorial = factorial * i
}

print (factorial)




#Ejercicio 4--------------------------------------------------------------------



v = c(2,4,1,3,6,7)
media = 0
n = 0

for (i in v){
  media = media + i
  n = n + 1
}

media = media / n

var = 0

for (i in v){
  
  var = var + (i^2 - media^2)
}

var = (1 / n) * var

print(var)




#Ejercicio 5-------------------------------------------------------------------



v = c(2,4,1,3,6,7)
sumapar = 0
sumaimpar = 0

for (i in 1:6){
  
  if (i %% 2 == 0){
    sumapar = sumap + v[i]
  }
  else{
    sumaimpar = sumaimpar + v[i] 
  }
}
print (sumapar - sumaimpar)