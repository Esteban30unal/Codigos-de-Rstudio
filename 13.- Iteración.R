#Introducción
#Hemos aprendido que una función nos permite simplificar una tarea que deseamos 
#realizar de manera repetida. En este capítulo nos enfocaremos en una herramienta 
#que nos proporcionará nuevos elementos para realizar una tarea de manera repetida.

#Comenzaremos con el uso de bucles, definiendo los elementos básicos que deben contener,
#desarrollaremos algunos ejemplos un tanto complejos y 
#aprenderemos herramientas de la librería purrr que nos ayudarán a 
#simplificar cada vez más determinados procesos. 
#La repetición de tareas, utiliza dos tipos programación 
#imperativa y funcional‘ en lo que sigue describimos cada una de ellas



library(tidyverse)

print(paste("El año es", 2010))

for (year in 2010:2019) {print(paste("El año es", year))}

for (i in 2010:2019) {print(paste("El año es", i))}

for (a in 1:9) {print(paste("El año es", a))}

for (a in 1800:1899) {print(paste("El año es", a))}

for (i in 1:10){
  if(!i %% 2){
    next
  } 
  print(i)
  }

for (i in 1:10) {
  if (!i %% 2){
    next} 
  print(i) 
  }

#2.1.- Mas For

install.packages("readxl")
library(readxl)
library(tidyverse)
datos<- read_excel("mu_enoe.xlsx")

attach(datos)
names(datos)
datos<- datos%>%
  select(edad, anios_esc, hrsocup, ingreso_mensual)
promedio<-datos%>% 
  summarise(
    edad=mean(edad, na.rm = TRUE), años_de_escolar=mean(anios_esc, na.rm = TRUE), 
    horas_ocupacion=mean(hrsocup, na.rm = TRUE), 
    ingreso_mensual=mean(ingreso_mensual, na.rm = TRUE), n=n())#Esto lo hice para mí

prom_edad<- mean(datos$edad)
prom_años_de_escolar<- mean(datos$anios_esc)
prom_horas_ocupacion<- mean(datos$hrsocup)
prom_ingreso_mensual<- mean(datos$ingreso_mensual)
resultados<-vector("double", 4)

for (i in 1:4) {
  resultados[[i]]=mean((datos[[i]]))
  }

for (i in 1:4) {
  resultados[[1]]=mean((datos[[1]]))
  } #Ejemplo de cómo funciona esta joda 

resultados<-vector("double", 4)

for (i in 1:4) {
  resultados[[i]]=mean((datos[[i]]))
  print(i)
  print(resultados)
}

map_dbl(datos, mean)#se verá más adelante

#3.- Programación Funcional


dp2 <- tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5),
  d = rnorm(5),
  e=a*b,
  f=c*d,
  g=c*f
)

medias <- vector("double",nrow(dp2))
for (i in 1:nrow(dp2)) {
  medias[[i]] <- mean(as.numeric(dp2[i,]))
} #[i,], significa toda la fila i, no está especificando una columna
medias


opera_fila<- function(x, func){
  res<-vector("double", nrow(x))
  for(i in 1:nrow(x)){
    res[[i]]<-func(as.numeric(x[i,]))
      
  }
  return(res)
}

opera_fila(dp2, mean)
opera_fila(dp2, var)
#4.- Iniciando con PURRR
#PURR
library(tidyverse)
dp <- tibble(
  a = rnorm(100, 5),
  b = rnorm(100, 6),
  c = rnorm(100, 7),
)


map_dbl(dp, median)#columnas 
map_dbl(dp2, median)

dp2%>%map_dbl(var) 
map_dbl(dp2, var)

funciones<-list(mean, median, var)
invoke_map(funciones, x=as.numeric(dp2[[3]]))



x<- list(a=1:10, b= 5:40, c=6:23)
y<-list(a=5:10, b= 20:40, c=7:12)
map(x, var)
map(y, var)
map_dbl(x, var)
map_dbl(y, var)

invoke_map(funciones, x=x[[1]])

z<- list(x=1:3, y=4:5)
map_int(z, length)


#5.- Atajos

library(readxl)
library(tidyverse)

datos<-read_excel("mu_enoe.xlsx")


datos%>%
  group_by(niv_edu)%>% count()

modelos<-datos%>%
  split(.$niv_edu)%>% #el "." significa datos
  map(~lm(ingreso_mensual~anios_esc, data=.))#lm=lineal model

modelos%>%
  map(summary)%>%
  map_dbl(~.$r.squared)

modelos%>%
  map(summary)%>%
  map_dbl(~.$coefficients[2,1])

#6.- Manejo de Errores

num <- list(50,30,70,90,"w")

raiz<- map(num, sqrt)#si hay un elemento que no se pueda hacer entonces no opera 

raiz<-map(num, safely(sqrt)) #safely si encuentra los errores lo describe.
raiz<-map(num, possibly(sqrt, NA))
str(raiz)
#7.- Iteraciones Multiples

library(tidyverse)

rnorm(10,100,20) 
mu<- list(5, 10, -3)
mu%>%
  map(rnorm, n=5)%>%
  str()
sigma<-list(1,5,10)


map2(mu,sigma, rnorm, n=5)%>%
  str()
N<- list(1,2,3)
args<-list(n, mu, sigma)
args%>% pmap(rnorm) %>% str()

attach(datos)
funciones<-c("mean", "median", "sum", "median")
datos_prueba<-select(
  datos, edad, anios_esc, hrsocup, ingreso_mensual)

invoke_map_dbl(funciones, datos_prueba)

funciones<-c("mean", "median", "iqr", "sd", "var")
datos_prueba<-select(
  datos, edad, anios_esc, hrsocup, ingreso_mensual)
estadiasticas<-map_dfr(
  datos_prueba, ~invoke_map_dbl(funciones,x=.))

graficas <- datos %>%
  split(.$niv_edu) %>%
  map(~ggplot(.,aes(ingreso_mensual, anios_esc))+
        geom_point())
graficas$`Primaria incompleta`
graficas$`Prrimaria completa`
graficas$`Secundaria completa`
graficas$`Medio superior y superior` 

# 8.- Otros Loops ---------------------------------------------------------
library(tidyverse)
datos %>% 
  keep(is.character) %>% 
  str()
datos_numericos <- datos %>% 
  discard(is.character)
datos$sex %>% 
  every(is.character)
combinado <- list(datos$sex, datos$edad)
combinado %>% every(is.character)
combinado %>% some(is.character)
combinado %>% some(is.numeric)

edades <- sample(combinado[[2]], 10)

edades %>%
  detect(~.>40)