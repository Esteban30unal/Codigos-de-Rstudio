library(tidyverse)
options(scipen = 999)
file.choose()

enoe<-read.csv("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Curso R\\Rstudio\\Archivos\\5.- Importaci�n\\mu_enoe2.csv")
enoe<-read.csv("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Curso R\\Rstudio\\Archivos\\5.- Importaci�n\\mu_enoe2.csv", 
               col.names =c("sexo", "Edad", "Ingreso"))
enoe
#3- Análisis de un vector
vector<-c("TRUE", "FALSE", "NA")
class(vector)
vector<- parse_logical(vector)
str(parse_logical(vector))

vector2 <- c("1","2","3")
class(vector2)

vector2<- parse_integer(vector2)


str(parse_integer(vector2))

vector3<-c("2010-01-01","1979-10-14")

vector3<-parse_date(vector3)

vector4<-c("1", "231", ".", "456")


parse_integer(vector4, na=".")


class(parse_integer(vector4, na="."))

vector4<-parse_integer(vector4, na=".")

#5-Números 

x<-"1.23"
y<-parse_double(x)
class(x)
class(y)
x
y

h<-"1,23"

h<-parse_double(h, locale = locale (decimal_mark=","))
t<-parse_double(h, locale = locale (decimal_mark=","))#para dejar el h correcto en otra constante

g<-"$100"
g<-parse_number(g)
k<-parse_number(g)

l<-"$442.185.895.145"
l<-parse_number(l, locale=locale(decimal_mark = ","))
remove(l,k,t,h,g,x,y)
#6.- Tiempos

x<-"2020/10/01"
x<-parse_date(x)#date pide los separadores
x

x<-"20201001"
x<-parse_datetime(x)

x<-"20201001T2010"
x<- parse_datetime(x)

x<-"09/12/20"
x<-parse_date(x, "%m/%d/%y")
x


x<- "05:30:05 pm"
x<-parse_time(x)
x

library(tidyverse)

prueba<-read_csv(readr_example("challenge.csv"))
file.choose()
attach(prueba)
prueba
problems(prueba)


prueba<-read_csv(readr_example("challenge.csv"), 
                 col_types = cols(
                   x=col_double(), 
                   y=col_date()
                                ))


prueba2 <- read_csv(readr_example("challenge.csv"),
                    col_types = cols(.default = col_character()))                                 

prueba2$y <-parse_date(prueba2$y)

class(prueba2$y)

prueba3<- read_csv(readr_example("challenge.csv"), 
                   guess_max = 1001
                   )

#8.- Exportar Datos
write.csv(prueba3, "challenge4.csv")
pruebacsv<- read_csv("challenge4.csv")
class(pruebacsv)
write_rds(prueba3, "challenge5.rds")

pruebards<-read_rds("challenge5.rds")
pruebards
class(pruebards)
class(pruebards$y)
class(pruebacsv$y)
