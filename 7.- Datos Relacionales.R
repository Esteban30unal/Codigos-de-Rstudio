#7.- Datos Relacionales
library(haven)
library(tidyverse)
hogares<- read_dta("hog_jal.dta")
viviendas<-read_dta("viv_jal.dta")
personas<-read_dta("per_jal.dta")
trabajos<-read_dta("trab_jal.dta")
ingresos<-read_dta("ing_jal.dta")

#3.- Llaves

viviendas%>% 
  count(folioviv)


viviendas%>% 
  count(folioviv) %>%
  filter(n>1)


hogares%>% 
  count(foliohog) %>% filter(n>1)

hogares%>% 
  count(folioviv,foliohog) %>% filter(n>1)


trabajos %>%
  count(folioviv, foliohog, numren, id_trabajo) %>%
  filter(n>1)
#4.- Joins


x<-tribble(~llave, ~sexo,~edad,
           1, "H","25",
           2, "H", "40",
           3, "H", "18")
y<- tribble(
  ~llave, ~ingreso, ~gasto,
  1, "1750", "1100",
  2, "2540", "1760",
  4, "3000", "890"
)

#4.2- Unión interior. La base nueva tiene las observaciones de los datos que coinciden 
#de las bases anteriores.

inner_join(x,y,by="llave")#"llave" es la clave en la que se unen las bases.

#4.3. Unión exterior: Diferentes tipos 
#1: Conserva observaciones de la izquierda y coloca NA 
#en los datos de las observaciones de la derecha 
left_join(x,y, by="llave")
#2:Conserva observaciones de la derecha y coloca NA
#en los datos de las observaciones de la izquierda 
right_join(x,y, by="llave")
#3:Conserva todas las observaciones 

full_join(x,y, by="llave")

#5.-Uniones Enigh

inner_join(ingresos, personas, by="folioviv")
ingresos_genero<-inner_join(ingresos, personas, by=c("folioviv", "foliohog", "numren"))#se coloca C por ser un vector. Varias columnas

ingresos_genero%>% count(sexo, clave)%>% 
  arrange(desc(n), sexo)
#como yo lo intenté hacer
Agua_mujeres<- inner_join(ingresos_genero, viviendas, by="folioviv")#ingreso tiene repetidos datos porque tienen más de un trabajo 
Agua_mujeres2<- left_join(ingresos_genero, viviendas, by="folioviv")

Agua_mujeres%>% count(sexo, dotac_agua)%>% arrange(desc(n), sexo)
#como era 
personas2<-select(personas, folioviv:sexo)


personas2<- left_join(personas2, viviendas, by="folioviv")

attach(personas2)
personas2 %>% count(sexo, dotac_agua)
personas2%>% count(sexo, dotac_agua)%>% arrange(desc(n), sexo)


#6.- Uniones de Filtro


personas_trabajan<- semi_join(personas, trabajos, by=c("folioviv", "foliohog", "numren"))

mean(personas_trabajan$edad)
summary(personas_trabajan$edad)
sd(personas_trabajan$edad)
quantile(personas_trabajan$edad, c(0.5))

personas_no_trabajan<- anti_join(personas, trabajos, by=c("folioviv", "foliohog", "numren"))

mean(personas_no_trabajan$edad)
summary(personas_no_trabajan$edad)
sd(personas_no_trabajan$edad)
quantile(personas_no_trabajan$edad, c(0.95))

#Personas que trabajan vs personas que no trabajan<---------------Es un ejemplo de un comentario del video
Nro_no_trabajan <- personas_no_trabajan %>% count(edad)
Nro_no_trabajan <- rename(Nro_no_trabajan, `No trabajan` = n)

Nro_trabajan <- personas_trabajan %>% count(edad)
Nro_trabajan <- rename(Nro_trabajan, `Trabajan` = n)

Resumen_trabajos <- full_join(Nro_no_trabajan, Nro_trabajan, by="edad")
Resumen_trabajos2 <- gather(Resumen_trabajos, `No trabajan` : `Trabajan`, key="Trabajan / No Trabajan", value = "Nro de personas")

ggplot(data=Resumen_trabajos2, mapping = aes(x=edad, y=`Nro de personas`))+
  geom_point(mapping = aes(color = `Trabajan / No Trabajan`)) +
  labs(title = "Personas que trabajan vs Personas que no trabajan", x= "Edad", y= "Nro de personas")


#7.- Operaciones de Conjuntos
personas_2<-select(personas, folioviv:numren)#toca las mismas cantidades de columnas
trabajos_2<-select(trabajos, folioviv:numren)

inter<-intersect(personas_2, trabajos_2)

union<-union(personas_2, trabajos_2)

set1<-setdiff(personas_2, trabajos_2)
