library(tidyverse)
library(lubridate)

today()
now()
class(today())
class(now())

fecha1<- "2021-01-05"
fecha2<-"january 05, 2021"
fecha3<-"05-jan-2021"

class(fecha1)
fecha1<- parse_date(fecha1)
fecha1<-ymd(fecha1)
fecha2<-mdy(fecha2)
fecha3<-dmy(fecha3)


hora1<-"2021-01-05 11:53:23"
hora2<-"05/01/2021 08:01"
hora1<-ymd_hms(hora1)
hora2<-ymd_hms(hora2)
class(hora1)

mibici<-read_csv("mibici.csv")


mibici2<-mibici%>% mutate(
  inicio=make_datetime(year, month, day, hour_inicio, minute_inicio, second_inicio))


mibici2<-mibici2%>% mutate(
  final=make_datetime(year, month, day, hour_final, minute_final, second_final))


mibici2<-mibici2%>% mutate(tiempo_viaje=final-inicio)

mibici2<-select(mibici2, -diferencia)
attach(mibici2)
promedio<-summarise(group_by(mibici2, fecha=make_datetime(year, month, day)), 
                    Promedio=mean(tiempo_viaje))


summary(promedio$Promedio)
sd(promedio$Promedio)
class(promedio$Promedio)

promedio$Promedio=as.numeric(promedio$Promedio)

shapiro.test(promedio$Promedio)

ggplot(promedio)+ geom_line(mapping = aes(fecha, Promedio), col="blue")

ggplot(promedio, aes(fecha, Promedio))+ geom_line(col="blue")

mibici2 %>%
  ggplot(aes(inicio))+
  geom_freqpoly()
mibici2 %>%
  ggplot(aes(inicio))+
  geom_freqpoly(binwidth=86400)


mibici2%>%
  filter(day==5) %>%
  ggplot() + geom_freqpoly(mapping = aes(inicio), binwidth=600)


fecha_prueba<- mibici2$inicio[1]
year(fecha_prueba)
month(fecha_prueba)
month(fecha_prueba, label=TRUE, abbr=FALSE) 
wday(fecha_prueba, label = TRUE, abbr = FALSE)

mibici2<-mibici2 %>% mutate(dia_sem=wday(inicio, label=TRUE, abbr=FALSE))

ggplot(mibici2) + geom_bar(mapping = aes(dia_sem), fill="blue", col="red")

mibici2<- mibici2 %>% mutate(edad=2024-nacimiento)
ggplot(mibici2)+ geom_bar(mapping = aes(edad), fill="blue", col="red")

#5.- Redondear Fechas


fecha_prueba
floor_date(fecha_prueba, "week")

mibici2%>% count(dia=floor_date(inicio, "day"))%>%
  ggplot(aes(dia, n))+
  geom_line()

inicio<-mibici2$inicio[1]
fin<-mibici2$final[1]
viaje<-fin-inicio
viaje

today()+ddays(7)
minutes(6)

hours(6)

hours(c(6, 3, 22))
mibici2<-mibici2 %>% mutate(duracion_propia=seconds_to_period(tiempo_viaje))



as.duration(tiem)
remove(semestre)
semestre<-"2023-08-08 14:00:00"
semestre<- ymd_hms(semestre)
class(semestre)
tiem<- now()- semestre
tiem
as.duration(tiem)

now<-now()
class(now)
now<- as.Date(now)
año<-"1998-10-10 00:00:00" 
año<-ymd_hms(año)
class(año)
tiem2<- now()-año
as.duration(tiem2)


#7.- Zona Horaria

Sys.timezone()
"America/Bogota"
Sys.setenv(tz="Europe/Madrid")
Sys.setenv(tz="America/Bogota")
now()

OlsonNames()

fecha_prueba2<-with_tz(fecha_prueba, tz="Europe/Madrid")
fecha_prueba2- fecha_prueba
