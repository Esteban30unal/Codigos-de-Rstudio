#Distribución de datos 
setwd("C:/Users/USUARIO/Desktop/Análisis estadstico/Curso R/Rstudio/Archivos")
library(tidyverse)
options(scipen = 999)
file.choose()
enigh<-read_csv("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Curso R\\Rstudio\\Archivos\\4.- Transformaci�n y Exploraci�n de Datos\\__MACOSX\\hogares_enigh.csv")
attach(enigh)


ggplot(datos=enigh) + geom_bar(mapping = aes(x=tot_integ))

enigh %>% count(tot_integ)

cantidad_integrantes<- group_by(enigh, tot_integ) %>% summarise(Total=n())

cantidad_integrantes

ggplot(datos=enigh)+ geom_histogram(mapping = aes(x=ing_cor), binwidth = 1000)

enigh %>% count(cut_width(ing_cor, 1000, boundary = 0))

ingresos<-filter(enigh, ing_cor<200000)
attach(ingresos)

ggplot(datos=ingresos)+ geom_histogram(mapping = aes(x=ing_cor), binwidth = 1000)

ingresos<-filter(enigh, ing_cor<100000)
attach(ingresos)

ggplot(datos=ingresos)+ geom_histogram(mapping = aes(x=ing_cor), binwidth = 1000)


#GSub 
ingresos$clase_hog<-gsub(1,"unipersonal", ingresos$clase_hog)
ingresos$clase_hog<-gsub(2,"nuclear", ingresos$clase_hog)
ingresos$clase_hog<-gsub(3,"ampliado", ingresos$clase_hog)
ingresos$clase_hog<-gsub(4,"compuesto", ingresos$clase_hog)
ingresos$clase_hog<-gsub(5,"correspondiente", ingresos$clase_hog)



h1<-ggplot(data =ingresos)+ geom_histogram(mapping = aes(x=ing_cor, color=clase_hog),
                                       binwidth = 1000)
group_by(ingresos, clase_hog) %>% summarise(Total=n()) #quería contar en número de personas por categoría 

h2<-ggplot(data =ingresos)+ geom_freqpoly(mapping = aes(x=ing_cor, color=clase_hog),
                                       binwidth = 1000)
install.packages("gridExtra")

library("gridExtra")
attach(ingresos)
grid.arrange(h1,h2,ncol=2)

attach(enigh)


#4 Valores típicos 
ggplot(datos=enigh)+ geom_histogram(mapping = aes(x=ingtrab), binwidth = 1000)

ggplot(datos=enigh)+ geom_histogram(mapping = aes(x=ingtrab), binwidth = 1000)+
  coord_cartesian(xlim = c(0, 150000))




ingresos_trabajo<-filter(ingresos, ingtrab>0,ingtrab<150000)
attach(ingresos_trabajo)

ggplot(datos=ingresos_trabajo)+ geom_histogram(mapping = aes(x=ingtrab), binwidth = 1000)

ingresos_trabajo$sexo_jefe<-gsub(1, "peneportantes",ingresos_trabajo$sexo_jefe)
ingresos_trabajo$sexo_jefe<-gsub(2, "cucaportantes",ingresos_trabajo$sexo_jefe)

attach(ingresos_trabajo)

ggplot(datos=ingresos_trabajo) + geom_freqpoly(mapping = aes(x=ingtrab,color=sexo_jefe), 
                                               binwidth=1000)
ggplot(datos=ingresos_trabajo) + geom_histogram(mapping = aes(x=ingtrab, color=clase_hog), 
                                               binwidth=1000)

Ejercicio<-group_by(ingresos_trabajo, clase_hog) 
attach(Ejercicio)
summary(Ejercicio)
summarise(Ejercicio, mean(ingtrab), sd(ingtrab))
class(Ejercicio)


#5.- Valores perdidos.
attach(enigh)

enigh<-mutate(enigh, ing_cor=ifelse(ing_cor==0 |ing_cor>200000,NA,ing_cor))


summary(enigh$ing_cor)

ggplot(datos=enigh) + geom_point(mapping = aes(x=edad_jefe, y=ing_cor))

ggplot(datos=enigh) + geom_point(mapping = aes(x=edad_jefe, y=ing_cor),na.rm = TRUE)

#5.- 6.- Covariación 

#2.6.1 Categórica con continua 

remove(ingresos, ingresos_trabajo, h1, h2, cantidad_integrantes)
  
  
summary(enigh$sexo_jefe)
summary(enigh$clase_hog)

enigh$sexo_jefe<-gsub(1,"Peneportante", enigh$sexo_jefe)
enigh$sexo_jefe<-gsub(2,"Cucaportante", enigh$sexo_jefe)
enigh$clase_hog<-gsub(1, "Unipersonal", enigh$clase_hog)
enigh$clase_hog<-gsub(2, "Nuclear", enigh$clase_hog)  
enigh$clase_hog<-gsub(3, "Ampliado", enigh$clase_hog) 
enigh$clase_hog<-gsub(4, "Compuesto", enigh$clase_hog)
enigh$clase_hog<-gsub(5, "corresidente", enigh$clase_hog)

summary(enigh$sexo_jefe)

attach(enigh)
ggplot(datos= na.omit(enigh), mapping = aes(x=ing_cor)) + geom_freqpoly( 
  mapping = aes(color=clase_hog), binwidth=10000)

ggplot(datos=enigh)+ geom_bar(mapping = aes(x=clase_hog), color="purple")
attach(enigh)

ggplot(datos=enigh)+ geom_bar(mapping = aes(x=clase_hog, y=..prop.., group=1), 
                              color="purple")

ggplot(data=na.omit(enigh), mapping = aes(x=ing_cor, y=..density..)) + 
  geom_freqpoly(mapping = aes(color=clase_hog), binwidth=10000)


enigh<-mutate(enigh,ingreso_per_capita = ing_cor/tot_integ)

attach(enigh)

ggplot(data=na.omit(enigh), mapping = aes(x=ingreso_per_capita, y =..density..))+
  geom_freqpoly(mapping = aes(color=clase_hog), binwidth=10000)

capita<- group_by(enigh, clase_hog)

class(capita)
summary(capita)
summarise(na.omit(capita), mean(ingreso_per_capita), sd(ingreso_per_capita))

ggplot(data=na.omit(enigh), mapping = aes(x=clase_hog, y =ingreso_per_capita))+
  geom_boxplot() + coord_flip()

summarise(group_by(na.omit(enigh), clase_hog), mean(ingreso_per_capita), 
          max(ingreso_per_capita), groups="drop")

summary(enigh$educa_jefe)

enigh$educa_jefe[1:5]
attach(enigh)
enigh<-mutate(enigh, educa_jefe =
                ifelse(educa_jefe=="01" , "Sin instrucción" ,
                       ifelse(educa_jefe=="02","Preescolar" ,
                              ifelse(educa_jefe=="03","Primaria incompleta" ,
                                     ifelse(educa_jefe=="04", "Primaria completa" ,
                                            ifelse(educa_jefe=="05","Secundaria incompleta" ,
                                                   ifelse(educa_jefe=="06", "Secundaria completa" ,
                                                          ifelse(educa_jefe=="07", "Preparatoria incompleta" ,
                                                                 ifelse(educa_jefe=="08", "Preparatoria completa" ,
                                                                        ifelse(educa_jefe=="09", "Profesional incompleta" ,
                                                                               ifelse(educa_jefe=="10", "Profesional completa" , "Posgrado"
                                                                               )))))))))))
enigh <- enigh %>%
  mutate(educa_jefe =
           ifelse(educa_jefe=="01" , "Sin instrucción" ,
                  ifelse(educa_jefe=="02","Preescolar" ,
                         ifelse(educa_jefe=="03","Primaria incompleta" ,
                                ifelse(educa_jefe=="04", "Primaria completa" ,
                                       ifelse(educa_jefe=="05","Secundaria incompleta" ,
                                              ifelse(educa_jefe=="06", "Secundaria completa" ,
                                                     ifelse(educa_jefe=="07", "Preparatoria incompleta" ,
                                                            ifelse(educa_jefe=="08", "Preparatoria completa" ,
                                                                   ifelse(educa_jefe=="09", "Profesional incompleta" ,
                                                                          ifelse(educa_jefe=="10", "Profesional completa" , "Posgrado"
                                                                          )))))))))))

attach(enigh)
ggplot(datos=enigh)+ geom_count(mapping = aes(x=clase_hog, y=educa_jefe))

edu_clase<-enigh %>% count(educa_jefe, clase_hog)

enigh %>% count(educa_jefe, clase_hog) %>% ggplot(mapping = aes(x=clase_hog, y=educa_jefe))+
  geom_tile(mapping = aes(fill=n))

enigh %>% count(educa_jefe, sexo_jefe) %>% ggplot(mapping = aes(x=sexo_jefe, y=educa_jefe))+
  geom_tile(mapping = aes(fill=n))
