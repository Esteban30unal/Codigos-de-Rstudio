
# 2.- Modelos con Purrr y Broom -------------------------------------------

f <- rnorm(10000, 100,15)
hist(f)
summary(f)
sd(f)

library(tidyverse)
library(modelr)
library(broom)
library(gridExtra)

setwd("C:/Users/USUARIO/Desktop/Análisis estadstico/Curso R/Rstudio/Archivos/15.- Modelos con Purr y Broom")

getwd()

file.choose()


datos <- read.csv("indicadores_banco_mundial.csv")


colnames(datos)

names(datos)[2] <-"pais" 
names(datos)[5] <-"esperanza" 
names(datos)[6] <-"pib_pc" 
names(datos)[1] <- "Nombre_país"
colnames(datos)

ggplot(datos)+geom_line(aes(tiempo, esperanza, group=pais, colour = pais))
ggplot(datos)+geom_line(aes(tiempo, esperanza, group=Nombre_país, colour = Nombre_país))

unique(datos$pais)
unique(datos$`país.Name`)
unique(datos$Nombre_país)
remove(n)
datos_mex <- datos %>% filter(pais=="MEX")
datos_col <- datos %>% filter(pais=="COL")
datos_col <- datos %>% filter(pais=="NIC")
datos_paises <- datos %>% filter(Nombre_país %in% c("Colombia","Uruguay","Estados Unidos", "Canadá"))
datos_col_mex <- datos %>% filter(Nombre_país %in% c("Colombia","México"))



ggplot(datos_paises)+geom_line(aes(tiempo, esperanza, group=Nombre_país, colour = Nombre_país), show.legend = FALSE)+
  facet_wrap(~Nombre_país, nrow = 1, ncol = 4)

ggplot(datos_col_mex)+geom_line(aes(tiempo, esperanza, group=Nombre_país, colour = Nombre_país))

ggplot(datos_col_mex)+geom_line(aes(tiempo, esperanza, group=pais, colour = pais))+
  facet_wrap(~pais, nrow = 2, ncol = 1)

g1 <- ggplot(datos_mex)+geom_line(aes(tiempo, esperanza, group=pais), alpha=1/2)
g1
ggplot(datos_col)+geom_line(aes(tiempo, esperanza, group=pais), alpha=1/3, colour="purple")


modelo_mex <-lm(esperanza~tiempo, data=datos_mex)
modelo_col <-lm(esperanza~tiempo, data=datos_col)
modelo_NIC <-lm(esperanza~tiempo, data=datos_NIC)

summary(modelo_mex)

datos_mex_predic <- datos_mex %>% add_predictions(modelo_mex)
datos_mex_resid <- datos_mex %>% add_residuals(modelo_mex)



g2 <- ggplot(datos_mex_predic)+geom_line(aes(tiempo, pred))
g2
ggplot(datos_mex_resid)+geom_line(aes(tiempo, resid))
g3<- ggplot(datos_mex_resid, aes(tiempo, resid))+
  geom_hline(yintercept = 0, color="White", size=3)+
  geom_line()+
  ggtitle("Patrón residual")
g3
grid.arrange(g1,g2,g3,ncol=3) 


# 3.- Datos Anidados ------------------------------------------------------

library(tidyverse)
library(modelr)
library(broom)
library(gridExtra)
attach(datos)
grupo_paises <- datos %>% 
  group_by(pais) %>% 
  nest()
modelo_pais <- function(df){
  lm(esperanza~pib_pc, data=df)
}

modelos <- map(grupo_paises$data, modelo_pais)

class(modelos)
length(modelos)

summary(modelos[[1]])
summary(modelos[[3]])

options(scipen=999)

options(scipen = 0)

grupo_paises_2 <- grupo_paises %>% 
  mutate(modelo=map(data, modelo_pais))
grupo_paises_2

grupo_paises_3 <- grupo_paises_2 %>% 
  mutate(resids=map2(data, modelo, add_residuals))

grupo_paises_4 <- grupo_paises_3 %>% 
  mutate(predict=map2(data, modelo, add_predictions))


residuos <- unnest(grupo_paises_3, resids)

r24 <- ggplot(residuos, aes(tiempo, resid))+
  geom_line(aes(group=pais), alpha=1/3)+
  geom_smooth(se=FALSE, colour="red", linewidth=0.3)

r24
attach(residuos)
r25 <- ggplot(residuos, aes(tiempo, resid))+
  geom_line(colour="purple3")+
  facet_wrap(~Nombre_país)
r25

filter(residuos, pais %in% c("NIC","HND", "HTI")) %>% 
  ggplot(aes(tiempo, resid))+
  geom_line(colour="purple3")+
  facet_wrap(~Nombre_país)

filter(residuos, pais=="NIC"|pais=="HND"|pais=="HTI")%>%
  ggplot(aes(tiempo, resid))+
  geom_line(colour="purple3")+
  facet_wrap(~Nombre_país)


# 4.- Calidad del Modelo con Broom ----------------------------------------

library(broom)

glance(modelo_mex)
glance(modelo_col)
glance(modelo_NIC)

grupo_paises_modelos <- grupo_paises_3 %>% 
  mutate(ajustes=map(modelo, glance)) %>% 
  unnest(ajustes)
