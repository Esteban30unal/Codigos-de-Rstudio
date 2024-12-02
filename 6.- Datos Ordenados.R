#6.- Datos Ordenados

#2.- Tidy Data

library(tidyverse)
library(readxl)
options(scipen = 999)
  tabla_1<- read_excel("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Curso R\\Rstudio\\Archivos\\6.- Datos Ordenados\\tablas_ejemplo.xls",
                     sheet="tabla1")
file.choose()

tabla_2<- read_excel("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Curso R\\Rstudio\\Archivos\\6.- Datos Ordenados\\tablas_ejemplo.xls",
                     sheet="tabla2")
tabla_3<- read_excel("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Curso R\\Rstudio\\Archivos\\6.- Datos Ordenados\\tablas_ejemplo.xls", 
                     sheet="tabla3")


#3.- Gathering Apila columnas en filas por variables claves 


datos<- read_csv("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Curso R\\Rstudio\\Archivos\\6.- Datos Ordenados\\datos_banco_mundial.csv")
remove(datos_banco_mundial)
file.choose()

datos2<-gather(datos, '1960':'2020', key="año",  value="valores")

#4.- Spreading  
remove(datos3)
datos3<-select(datos2, -('serie Code'))
datos4<-spread(datos3, key='serie Name', value = 'valores')

datos3<- datos2 %>% select(-('serie Code')) %>%spread(key='serie Name', value = 'valores')

attach(datos3)
`Cajeros automaticos (por cada 100.000 adultos)`
datos <- datos3 %>%
  mutate(`Cajeros automaticos (por cada 100.000 adultos)` = ifelse
         (`Cajeros automaticos (por cada 100.000 adultos)`==".." , NA,
           `Cajeros automaticos (por cada 100.000 adultos)`) ) %>%
  mutate(`PIB (US$ a precios constantes de 2010)` = ifelse
         (`PIB (US$ a precios constantes de 2010)`==".." , NA,
           `PIB (US$ a precios constantes de 2010)`) ) %>%
  mutate(`Poblacion, total` = ifelse
         (`Poblacion, total`==".." , NA,
           `Poblacion, total`) )
summary(datos)
class(datos$`Cajeros automaticos (por cada 100.000 adultos)`)
datos<-mutate(datos, Años=as.numeric(año))
datos<- datos%>%mutate(`Cajeros automaticos (por cada 100.000 adultos)`=as.numeric(`Cajeros automaticos (por cada 100.000 adultos)`))%>% 
  mutate(`PIB (US$ a precios constantes de 2010)`=as.numeric(`PIB (US$ a precios constantes de 2010)`))%>%
  mutate(`Poblacion, total`=as.numeric(`Poblacion, total`))
summary(datos)
attach(datos)
summary(`PIB (US$ a precios constantes de 2010)`)

ggplot(datos) + geom_line(mapping = aes(año, `PIB (US$ a precios constantes de 2010)`, group=Pais), color="Grey50", na.rm = TRUE)+ 
  geom_point(mapping =aes(año, `PIB (US$ a precios constantes de 2010)`, color=Pais), na.rm = TRUE)

ggplot(datos, aes(periodo, `PIB (US$ a precios constantes de 2010)`))+
  geom_line(aes(group = Pais), color = "grey50", na.rm = TRUE) +
  geom_point(aes(color = Pais), na.rm = TRUE)

#

file.choose()

trabajo<-read_csv("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Curso R\\Rstudio\\Archivos\\6.- Datos Ordenados\\Investigadores-SNI-Vigentes-2018.csv")
sni<-trabajo 
remove(trabajo)
sni<-sni%>% unite(nombre, `Apellido Paterno`, `Apellido Materno`, Nombre, sep=" ")
sni<-sni%>% 
  separate( `Area del Conocimiento`,into=c("cve_area", "area"), sep = 
              ":")
sni <- sni %>%
  separate(`Area del Conocimiento`, into = c("cve_area", "area"), sep =
             ":")  
  

  
  
  
  