#8.- Cadenas de Texto



library(tidyverse)
library(htmlwidgets)
library(readxl)
library(haven)


a<-"\"Hola\""
a

writeLines(a)


b<-"\\Hola"
writeLines(b)

c<-"Hola \n saludos"
writeLines(c)

d<-"Hola \t saludos"
writeLines(d)

h<-c("hola", "mundo", "saludos")
writeLines(h)
z<-"\u00b5"
w<-"\u0026"
z
w


x<-"hola"
y<-"mundo"
str_c(x," ", y)

ñ<-str_c(x,y, sep = ",")
ñ
n<-str_c(x,y, collapse = ",")
n
z<-str_c(h, collapse = " ")
z
nombre<- "Arturo"
hora<-"mañana"
cumple<-TRUE

str_c("buena", " ",hora, " ",nombre)

str_c("Buena", " ",hora, ", ",nombre, ". ",if(cumple) "Feliz cumpleaños")
x <- c("0014525", "0024685", "0028596")
x
str_sub(x, 1, 3)

str_sub(x, -4, -1)


str_to_lower("HOLA")

str_to_upper("hola")

str_to_title("hola")


x<-c("Anual", "Trimestral", "Semestral", "Uso", "Equipo")
str_sort(x)

# 5.- Identificando Patrones

str_view(x, "a")

x <- c("arroz", "arma", "azúcar", "arte", "turca", "martes", "marzo")


str_view(x, "ar")

str_view(x, "r.")
str_view(x, ".a.")

y<-c("pastel de manzana", "manzana", "jugo de manzana", "pure de manzana")
str_view(y, "manzana")


h<-c("a3bc", "a.c", "a*c", "a c", "abc", "a5", "b*c")
str_view(h, "a\\d")
str_view(h, "a\\s")
str_view(h, "[abc]")

todos<-read_excel("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Curso R\\Rstudio\\Archivos\\8.- Cadenas de Texto\\def_ingresos.xlsx")
todos<-read_excel("def_ingresos.xlsx")

library(haven)

ingresos<-read_dta("ing_jal.dta")
attach(ingresos)
ing_in<- todos%>% mutate(clave=str_sub(CLAVE, 1, 4))%>% 
  filter(str_detect(todos$CLAVE,"^P0")==TRUE)


ing_in<- ing_in %>% 
  mutate(nombre=str_sub(CLAVE, 6))%>% 
  select(-CLAVE)
ingres_con_concepto<-left_join(ingresos, ing_in, by= "clave")
