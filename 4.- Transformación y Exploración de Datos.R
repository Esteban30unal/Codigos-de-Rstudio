setwd("C:/Users/USUARIO/Desktop/Análisis estadstico/Curso R/Rstudio/Archivos")

library(tidyverse)
library(ggplot2)
#Para evitár anotación científica
options(scipen=999)
file.choose()

enigh<-read.csv("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Curso R\\Rstudio\\Archivos\\4.- Transformaci�n y Exploraci�n de Datos\\__MACOSX\\hogares_enigh.csv")
attach(enigh)
names((enigh))
objects()
remove(enoe)
colnames(enigh)

filter(enigh, clase_hog==2)

clases_hogares<-filter(enigh, clase_hog==2)

dim((clases_hogares))

hogares_nucleares_femen<-filter(enigh, clase_hog==2, sexo_jefe==2)

hogares_femen<-filter(enigh, (clase_hog==1|clase_hog==2), sexo_jefe==2)
hogares_nucleares_masc<-filter(enigh, clase_hog==1, sexo_jefe==1)


arrange(enigh, edad_jefe)

colnames(enigh)
enigh_corto<-select(enigh, folioviv, foliohog, ing_cor, gasto_mon, 
                    tot_integ, ubica_geo, sexo_jefe, clase_hog, edad_jefe, educa_jefe)

enigh_corto2<-select(enigh, foliohog:sexo_jefe)

enigh_corto2<-select(enigh, foliohog:sexo_jefe, gasto_mon)

enigh_corto2<-select(enigh_corto2, -(gasto_mon))

select(enigh, starts_with("fol"))

select(enigh, ends_with("s"))

enigh_corto<-mutate(enigh_corto, ingreso_capita= ing_cor/tot_integ)

enigh_corto<-mutate(enigh_corto, cve_ent=substr(ubica_geo, 1,2))

enigh_3<-select(enigh, ubica_geo)

enigh<-mutate(enigh, cve_ent=substr(ubica_geo, 1,2))

enigh<-select(enigh,-(cve_ent))

remove(enigh_3)
enigh_3<-select(enigh, folioviv)
colnames(enigh)
enigh_corto<-mutate(enigh_corto, ingreso_corriente=log(ing_cor), gasto_monetario=log(gasto_mon),
              gasto_porcentaje=gasto_mon/ing_cor*100)

enigh_corto<-select(enigh, folioviv, foliohog, ing_cor, gasto_mon, 
                    tot_integ, ubica_geo, sexo_jefe, clase_hog, edad_jefe, educa_jefe)

enigh_corto<-mutate(enigh_corto, ingreso_capita=ing_cor/tot_integ)

remove(gasto_porcentaje)              

enigh<-select(enigh, -(ingreso_corriente), -(gasto_monetario), -(gasto_porcentaje))

enigh_corto<-mutate(enigh_corto, ingreso_corriente=log(ing_cor), gasto_monetario=log(gasto_mon), 
              gasto_porcentaje=((gasto_mon/ing_cor)*100))
colnames(enigh)
              
summary(enigh$ing_cor)

summary(enigh$gasto_mon,)

summarise(enigh, mean(ing_cor), mean(gasto_mon))

summarise(enigh, median(ing_cor), median(gasto_mon))

summarise(enigh, sd(ing_cor), sd(gasto_mon))

#8 GroupBy
#1=hombres, 2=mujeres
summarise(group_by(enigh, sexo_jefe), mean(ing_cor), mean(gasto_mon))#1/2 formas de hacerlo

sexo<- group_by(enigh_corto, sexo_jefe) #2/2 forma de hacerlo. 
#Ahora los summarise son con las agrupacionas que queramos
class(sexo)
summarise(sexo, mean(ing_cor), mean(gasto_mon))

clases_hogar<-group_by(enigh_corto, sexo_jefe, clase_hog)
summarise(clases_hogar, mean(ing_cor), mean(gasto_mon))

enigh_corto<-mutate(enigh_corto, ingreso_capita=ing_cor/tot_integ)
enigh_corto<-mutate(enigh_corto, ingreso_capcop=((ingreso_capita/3)*230.64))

Ejercicio<-filter(enigh_corto, ing_cor>0, gasto_mon>0)

edad<-group_by(Ejercicio, edad_jefe)
attach(edad)

summarise(edad, mean(ingreso_capita), mean(ingreso_capcop), mean(educa_jefe))
mode(educa_jefe)

edad$educa_jefe <- as.numeric(edad$educa_jefe)

ingreso_edad<-summarise(edad, mean(ingreso_capita))
ingreso_edad
ingreso_edad<-summarise(edad, ingreso=mean(ingreso_capita))
ingreso_edad
ingreso_edad<-summarise(edad, ingreso=mean(ingreso_capita), numero_hogares=n(), estudio=mean(educa_jefe))
ingreso_edad
ingreso_edad<-summarise(edad, ingreso=mean(ingreso_capita), ingreso_cop=mean(((ingreso_capita/3)*230.64)),numero_hogares=n())
ingreso_edad
attach(ingreso_edad)
ggplot(data=ingreso_edad, mapping = aes(x=edad_jefe, y=ingreso_cop)) + geom_point(aes(size=numero_hogares), alpha=1/3) + geom_smooth(se=FALSE)

#9-Pipe. Esto evita un momntón de pasos. 

enigh_corto$educa_jefe <- as.numeric(enigh_corto$educa_jefe)

ingreso_edad2<- filter(enigh_corto, ing_cor>0, gasto_mon>0) %>%
  group_by(edad_jefe) %>% summarise(mean(ingreso_capita), hogares=n() ,mean(ingreso_capcop)
                                    ,mean(educa_jefe))
ingreso_edad2

hogar<-filter(enigh_corto, ing_cor>0, gasto_mon>0) %>% group_by(cve_ent, clase_hog) %>%
summarise(ingreso_cap=mean(ingreso_capita), cantidad_hogares=n())

hogar

ggplot(datos=hogar) + geom_bar(mapping = aes(x=cve_ent, y= ingreso_cap, fill=clase_hog), 
                               stat = "identity", position="dodge") 
hogar2<-filter(enigh_corto, ing_cor>0, gasto_mon>0) %>% group_by(cve_ent) %>%
  summarise(ingreso_cap=mean(ingreso_capita), cantidad_hogares=n())
hogar2
                               
ggplot(datos=hogar, mapping = aes(x=cve_ent,fill=clase_hog))+ geom_bar(position = "dodge") + 
  labs(title = "Región y clase de hogar", x="Ubicación", y="no_sé")

attach(hogar)
mode(clase_hog)
enigh_corto$educa_jefe <- as.numeric(enigh_corto$educa_jefe)
hogar$clase_hog<-as.character(hogar$clase_hog)
mode(hogar$clase_hog)
