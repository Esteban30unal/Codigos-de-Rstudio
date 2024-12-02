library(tidyverse)


letters
x<-seq(1:1000)
x
length(x)
class(x)

y<- list("a", "b",1:100) #Lista es un vector del 1 al 100/ el 1:100 es un elemento/secuencia

y
class(y)

z<-list("Juan", "Maria", "Carmen")

class(z)

length(y)
length(x)

y[[2]] 

y[[3]]


x#vector atómico

x[2]
x[6]


y[1]
y[3][1]#Así está mal 
y[[3]][1]


y[[3]][60]


#3.- Vector Atómico/ Hay 8 tipos
x<-c(10,20,40,80, 90,200, 500, 600, 700, 800, 1000)
y<-(x>100 & x<1000)#vectores lógicos
y


class(1)
class (1L)

x<-c(1,5,6)

class(x)
typeof
x<- as.integer(x)
class(x) 

sum(y)

mean(y)

a<-c(1,1,1,1,1,1)
a2<-c(2,2,2,2,2,2) 
a3<-a+a2
a3
b<-c(1,2,3,4,5)
b2<-10
b+b2
b2<-c(10, 15)

b+b2

b<-c(1,2,3,4,5,6)
b2<-c(10, 15)

b+b2


x<-c(1,2,3)
set_names(x, c("a", "b", "c"))
x<-c(alpha=1, beta=2, gamma=3)
x
vector_completo<-c(10, 20,30,40,50,60,70,80,90,100)

vector_completo[1:3]
vector_completo[c(7,8)]
vector_completo[c(1:3,7,8)]

#4.- Vectores de Listas

estudiantes<-list(c("Maria","Juan","Pedro", "Carmen", "Magnolia", "Arturo"),
                  c(20,25,28,28,25,29))
estudiantes

set_names(estudiantes, "Nombre", "Edad")
str(estudiantes)
estudiantes[[1]][3]

#5.- Último elemento de un vector

tail(vector_completo, 1)

tail(vector_completo, 3)

vector_completo[length(vector_completo)]
