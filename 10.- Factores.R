#2.- Forcats
file.choose()

library(tidyverse)
library(forcats)
lapop<- read.csv("lapop_mexico.csv")
remove(lapop_mexico)
names(lapop)[9:10] <- c("region", "entidad")
names(lapop)[16:18] <- c("sexo", "edad", "problema_mas_grave")
names(lapop)[22] <- "voto"
names(lapop)[25:27] <- c("confianza_politica", "elecciones", "ingreso")

summary(lapop)


lapop<- lapop%>% 
  mutate(region=as.factor(region), entidad=as.factor(entidad),
         sexo=as.factor(sexo), voto=as.factor(voto), 
         problema_mas_grave=as.factor(problema_mas_grave),
         confianza_politica=as.factor(confianza_politica), 
         elecciones=as.factor(elecciones), ingreso=as.factor(ingreso))

summary(lapop)
lapop%>% count(region)
lapop%>% count(edad)
lapop%>% count(voto)
ggplot(lapop) + geom_bar(mapping = aes(region), fill="purple", col="green")


#3.- Modificar Factores

remove(votos)
votos<- lapop %>% group_by(voto) %>% summarise(edad_promedio= mean(edad, na.rm = TRUE), n=n())

votantes<- group_by(lapop, voto)

names(votos)

summarise(votantes, mean(edad))
attach(votos)
ggplot(votos)+ geom_point(mapping = aes(edad_promedio, voto),
                          size=3, color="purple")

ggplot(votos)+ geom_point(mapping = aes(n, voto),
                          size=3, color="purple")


ggplot(votos, aes(edad_promedio, fct_reorder(voto, edad_promedio)))+
         geom_point(size=3, col="purple")
ggplot(votos)+ 
  geom_point(mapping = aes(edad_promedio, fct_reorder(voto, edad_promedio)), 
             size=3, col="purple")


ggplot(votos)+ geom_point(mapping = aes(n, voto),
                          size=3, color="purple")

ggplot(votos, aes(n, fct_reorder(voto, n)))+
  geom_point(size=3, col="purple")
ggplot(votos)+ 
  geom_point(mapping = aes(n, fct_reorder(voto, n)), 
             size=3, col="purple")



ingreso_edad<- lapop %>%  group_by(ingreso) %>% 
  summarise(edad_promedio= mean(edad, na.rm = TRUE), n=n())

summarise(edad_promedio)



ggplot(ingreso_edad) + 
  geom_point(mapping = aes(edad_promedio, ingreso), size=3, col="purple")

ggplot(ingreso_edad)+ 
  geom_point(mapping = aes(edad_promedio, fct_reorder(ingreso, edad_promedio)),
             size=3, col="purple")

ggplot(ingreso_edad, aes(edad_promedio, fct_reorder(ingreso, edad_promedio)))+
  geom_point(size=3, col="purple")


ggplot(ingreso_edad)+ 
  geom_point(mapping = aes(
    edad_promedio,fct_relevel(
      ingreso, "No sabe","No responde", "Ningún ingreso")),
    size=3, col="purple")


ggplot(ingreso_edad)+ 
  geom_point(mapping = aes(
    edad_promedio,fct_relevel(fct_reorder(
      ingreso, edad_promedio), "No sabe","No responde", "Ningún ingreso")),
    size=3, col="purple")


lapop%>% mutate(region=region%>% fct_infreq()%>% fct_rev()) %>% 
  ggplot() + geom_bar(mapping = aes(region),fill="blue") #Uno lo ordena y el otro de menor a mayor 

#4.- Cambiar el orden de los Factores

attach(lapop)

lapop2<- lapop %>%
  mutate(voto=fct_recode(voto, "AMLO"="Andrés Manuel López Obrador Morena Juntos Haremos Historia"
                         ,
                         "Bronco" = "Jaime Rodríguez Calderón El Bronco Independiente",
                         "Meade" = "José Antonio Meade Kuribreña PRI",
                         "Anaya" = "Ricardo Anaya Cortés PAN",
                         "NR, NS" = "No responde",
                         "NR, NS" = "No sabe",
                         "Anulado" = "Ninguno (anuló su voto)",
                         "Anulado" = "Ninguno (fue a votar pero dejó la boleta en blanco)"))


summary(lapop2$voto)


lapop2%>% mutate(voto=voto%>% fct_infreq()) %>%
  ggplot() + geom_bar(mapping = aes(voto), fill="purple")


lapop %>%
  mutate(problema_mas_grave = problema_mas_grave %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(problema_mas_grave)) +
  geom_bar(fill="red") + coord_flip()


lapop %>%
  mutate(problema_mas_grave = fct_lump(problema_mas_grave, n=10) %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(problema_mas_grave)) +
  geom_bar(fill="red") + coord_flip()


lapop %>%
  mutate(problema_mas_grave = fct_lump(
    problema_mas_grave, n=10, other_level = "Los demás") %>% 
      fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(problema_mas_grave)) +
  geom_bar(fill="red") + coord_flip()
  