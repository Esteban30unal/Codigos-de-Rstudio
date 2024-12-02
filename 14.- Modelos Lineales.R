

install.packages("modelr")
install.packages("hexbin")
install.packages("splines2")

library(tidyverse)
library(modelr)
library(hexbin)
library(splines2)


# 2.- Análisis de Regresión -----------------------------------------------


x1<-rnorm(500, mean=10, sd=10)
x2<-rnorm(500, mean=20, sd=20)
u<-rnorm(500, mean=0, sd=1)


 y <- 2+(5*x1)+(10*x2)+u

datos <- tibble(y=y, x1=x1, x2=x2)
 
 view(datos)
 x3 <- x1+x2
 view(x3)
 mean(x3)
 sd(x3)

datos <- tibble(y=y, x1=x1, x2=x2, x3=x3)
view(datos)


grafica <-  ggplot(datos)+ geom_point(aes(x3,y))
grafica2 <-datos %>% ggplot(.,aes(x3,y))+geom_point()#Así está en el video
ggplot(datos)+ geom_point(aes(x3,y))

grafica
grafica2

reg <- lm(y~x1+x2,data=datos)

summary(reg)


# 3.- Factores Categóricos  -----------------------------------------------


genero<-round(runif(500,min = 0, max = 1))

view(genero)

y_2 <- 2+(5*x1)+(10*x2)+ (5*genero) + u

datos_genero <- tibble(y_2=y_2, x1=x1, x2=x2, sexo=genero)

view(datos_genero)
x_3 <- x1+x2+genero
x_3 <- x1+x2+sexo

view(x_3)
datos_genero2 <- tibble(y_2=y_2, x1=x1, x2=x2, sexo=genero, x_3=x_3)

view(datos_genero2)
attach(datos_genero2)
grafica_genero <- ggplot(datos_genero2)+geom_point(aes(x_3,y))
grafica_genero

reg2 <- lm(y_2~x1+x2+sexo, data=datos_genero2)

summary(reg2)

options(scipen=999)

options(scipen = 0)

datos_genero <- datos_genero %>% mutate(sexo2=as.character(sexo)) %>% 
  mutate(sexo2=fct_recode(sexo2, 
                          "hombre"="0",
                          "mujer"="1"
                          ))#para renombrar variables cualitativas deben estar como character


class(datos_genero$sexo)

class(datos_genero$sexo2)

reg3 <- lm(y_2~x1+x2+sexo2, data=datos_genero)

summary(reg3)

c <- c("a","b","c","d")

datos_genero <- tibble(datos, categoria=rep(c,125))#imagino que no hay que declarar la base donde está c porque es únicamente un dato explicito

x_3 <- x1+x2+genero
reg4 <- lm(y_2~x1+x2+categoria, data=datos_genero)#la variable categoria no tiene una correlación con las otras variables ya que es aleatoria y  no se a hecho ninguna operación con y_2 para forzar la relación. 

summary(reg4)


# 4.- Ajuste del Modelo ---------------------------------------------------

predict <- datos_genero %>% mutate(prediccion=add_predictions(reg3)) %>% 
 mutate(residuos= add_residuals(reg3))

predict <- datos_genero %>% add_predictions(reg3) %>% 
  add_residuals(reg3)

hist(predict$resid)
hist(predict$pred)
library(nortest)
lillie.test(x = predict$pred)
lillie.test(x = predict$resid)


ggplot(predict) + geom_point(aes(x1,resid))+
  geom_point(aes(x1,resid)) +
  geom_smooth(aes(x1,resid), color="red", se=FALSE)#Como yo lo hice

predict %>% ggplot(aes(x1,resid))+ 
  geom_point()+
  geom_point()+
  geom_smooth(color="red", se=FALSE)#video


cov(predict$x1,predict$resid)

predict %>% ggplot(aes(x2,resid))+ 
  geom_point()+
  geom_point()+
  geom_smooth(color="red", se=FALSE)#video
  
cov(predict$x2,predict$resid)


predict %>% ggplot(aes(pred,resid))+ 
  geom_point()+
  geom_point()+
  geom_smooth(color="red", se=FALSE)#video


cov(predict$pred,predict$resid)

cor(datos_genero[,2:4])

cor(datos[,2:4])


# 5.- Enigh p1 ------------------------------------------------------------

library(tidyverse)
library(readr)
getwd()#Para ver el directorio de trabajo 
file.choose()
setwd(
"C:/Users/USUARIO/Desktop/Análisis estadstico/Curso R/Rstudio/Archivos/14.- Modelos")# Para asignar el directorio 
enigth <- read.csv("hogares_enigh.csv")
remove(enigth)
remove(enigth_2)
head(enigth)
class(enigth)
class(datos)

enigth_2 <- enigth %>% mutate(sexo_jefe=as.factor(sexo_jefe)) %>% 
  mutate(clase_hog=as.factor(clase_hog)) %>%
  mutate(educa_jefe=as.factor(educa_jefe)) %>% 
  
  mutate(sexo_jefe=fct_recode(sexo_jefe,
    "hombre"="1", 
    "mujer"="2")) %>% 
  mutate(clase_hog=fct_recode(clase_hog,
    "unipersonal"="1",
    "nuclear"="2",
    "ampliado"="3",
    "compuesto"="4",
    "corresidente"="5")) %>% 
  mutate(educa_jefe=fct_recode(educa_jefe,
    "sin instruccion"="1",
    "preescolar"="2",
    "primaria incompleta"="3",
    "primaria completa"="4",
    "secundaria incompleta"="5",
    "secundaria completa"="6",
    "preparatoria incompleta"="7",
    "preparatoria completa"="8",
    "profesional incompleta"="9",
    "profesinal completa"="10",
    "posgrado"="11"))
remove(enigth_2)

options(scipen=999)

ggplot(enigth_2)+geom_point(aes(x=ing_cor, y= alimentos))
attach(enigth_5)
ggplot(enigth_5)+geom_point(aes(x=ingresos_cop, y= alimentos_cop))
  

enigth_5 <- enigth_5 %>% 
  mutate(ingresos_cop=ifelse(ingresos_cop==0,NA, ingresos_cop)) %>% 
  mutate(alimentos_cop= ifelse(alimentos_cop==0,NA, alimentos_cop)) %>% 
  filter(ingresos_cop!="NA") %>% 
  filter(alimentos_cop!="NA")#!= significa diferente

enigth_5 <- enigth_5 %>% 
  mutate(ln_alimentos_pesos=log(alimentos_cop)) %>% 
  mutate(ln_ingreso_pesos=log(ingresos_cop))#¿Por qué será conveniente usar el logaritmo?

ggplot(enigth_5)+geom_point(aes(x=ln_ingreso_pesos, y=ln_alimentos_pesos))


enigth_3 <- enigth_2 %>% 
  mutate(ing_cor=ifelse(ing_cor==0,NA, ing_cor)) %>% 
  mutate(alimentos= ifelse(alimentos==0,NA, alimentos)) %>% 
  filter(ing_cor!="NA") %>% 
  filter(alimentos!="NA")#!= significa diferente

enigth_4 <- enigth_3 %>% 
  mutate(ln_alimentos=log(alimentos)) %>% 
  mutate(ln_ingreso=log(ing_cor))#¿Por qué será conveniente usar el logaritmo?

ggplot(enigth_4)+geom_point(aes(x=ln_ingreso, y=ln_alimentos))


# 6.- Scatterplots --------------------------------------------------------


install.packages("hexbin")
install.packages("splines2")
install.packages("moments")
library(tidyverse)
library(hexbin)
library(splines2)
library(moments)

ggplot(enigth_4)+geom_hex(aes(x=ln_ingreso, y=ln_alimentos), bins=50) #Bins=cada cuanto aumenta el color

ggplot(enigth_5)+geom_hex(aes(x=ln_ingreso_pesos, y=ln_alimentos_pesos), bins=50) #Bins=cada cuanto aumenta el color

ggplot(enigth_4, aes(x=ln_ingreso, y=ln_alimentos)) + geom_hex(bins=50)#este es el código del video
  
ggplot(enigth_4)+geom_point(aes(x=ln_ingreso, y=ln_alimentos, 
                                color=clase_hog))

ggplot(enigth_5)+geom_point(aes(x=ln_ingreso_pesos, y=ln_alimentos_pesos, 
                                color=clase_hog))


ggplot(enigth_4,aes(x=ln_ingreso, y=ln_alimentos))+
  geom_point()+
  geom_point(aes(color=clase_hog))+ 
  # NO USAR(MUY LENTO)es el del video, no veo mucha diferencia colocando dos Geom_poin/NO USAR(MUY LENTO)
  

attach(enigth_4)
remove(enigth_4)

ggplot(enigth_4)+geom_point(aes(x=ln_ingreso, y=ln_alimentos, 
                                  color=educa_jefe))
ggplot(enigth_5)+geom_point(aes(x=ln_ingreso_pesos, y=ln_alimentos_pesos, 
                                  color=educa_jefe))

ggplot(enigth_4)+geom_point(aes(x=ln_ingreso, y=ln_alimentos, 
                                color=sexo_jefe))

ggplot(enigth_5)+geom_point(aes(x=ln_ingreso_pesos, y=ln_alimentos_pesos, 
                                color=sexo_jefe))
# 7.- Graficas de caja y modelo -------------------------------------------

sumg <- enigth_4 %>% 
  group_by(clase_hog) %>%  summarise(ln_alimentos=mean(ln_alimentos))

sumg_5 <- enigth_5 %>% 
  group_by(clase_hog) %>%  summarise(ln_alimentos_pesos=mean(ln_alimentos_pesos))

sumg <- enigth_4 %>% group_by(clase_hog)#Así es como se hace por partes
sumg <- sumg %>%  summarise(ln_alimentos=mean(ln_alimentos))


ggplot(enigth_4, aes(clase_hog, ln_alimentos, color=clase_hog))+
  geom_boxplot()+
geom_point(data=sumg, color="black", size=4)#asi está en el video
  
ggplot(enigth_5, aes(clase_hog, ln_alimentos_pesos, color=clase_hog))+
  geom_boxplot()+
geom_point(data=sumg_5, color="black", size=4)#asi está en el video
  
ggplot(enigth_4, aes(clase_hog, ln_alimentos, color=clase_hog))+
  geom_boxplot()#se ve más bonito sin el Geom_point

ggplot(enigth_4)+
  geom_boxplot(aes(clase_hog, ln_alimentos, color=clase_hog))+
  geom_point((aes(clase_hog, ln_alimentos, color=clase_hog)),data=sumg, color="black", size=4) #así lo hice yo para hacer el camino largo


sumg2 <- enigth_4 %>% 
  group_by(educa_jefe) %>% 
  summarise(ln_alimentos=mean(ln_alimentos))

sumg2_5 <- enigth_5 %>% 
  group_by(educa_jefe) %>% 
  summarise(ln_alimentos_pesos=mean(ln_alimentos_pesos))

ggplot(enigth_4, aes(educa_jefe, ln_alimentos, color=educa_jefe))+
  geom_boxplot()+
geom_point(data=sumg2, color="black", size=4)#asi está en el video

ggplot(enigth_4, aes(educa_jefe, ln_alimentos_pesos, color=educa_jefe))+
  geom_boxplot()+
geom_point(data=sumg2_5, color="black", size=4)#asi está en el video


ggplot(enigth_5)+
  geom_boxplot(aes(educa_jefe, ln_alimentos_pesos, color=educa_jefe))+
  geom_point((aes(educa_jefe, ln_alimentos_pesos, color=educa_jefe)),data=sumg2_5, color="black", size=4)


ggplot(enigth_4, aes(educa_jefe, ln_alimentos, color=educa_jefe))+
  geom_boxplot()+
geom_point(data=sumg2, color="black", size=4)+
  coord_flip()
attach(enigth_5)
ggplot(enigth_5, aes(educa_jefe, ln_alimentos_pesos, color=educa_jefe))+
  geom_boxplot()+
geom_point(data=sumg2_5, color="black", size=4)+
  coord_flip()

enigth_5 <- enigth_4 %>% 
  mutate(alimentos_cop=alimentos*211,48) %>% mutate(ingresos_cop= ing_cor*211,48 )


  
modelo <- lm(ln_alimentos~ln_ingreso+tot_integ + edad_jefe+sexo_jefe+clase_hog+ educa_jefe, data=enigth_4)

modelo2 <- lm(ln_alimentos_pesos~ln_ingreso_pesos+tot_integ + edad_jefe+sexo_jefe+clase_hog+ educa_jefe, data=enigth_5)


summary(modelo, digits=5)

summary(modelo2, digits=5)


predict <- enigth_4 %>% add_predictions(modelo) %>% add_residuals(modelo)
mean(predict$resid)
hist(predict$resid)

predict2 <- enigth_5 %>% add_predictions(modelo2) %>% add_residuals(modelo2)

mean(predict2$resid)
hist(predict2$resid)



predict %>% ggplot(aes(x=pred, y=resid^2))+geom_point()+geom_smooth(color="red",se=FALSE)


# 8.- Entradas de Visitantes a México -------------------------------------
library(tidyverse)
library(hexbin)
library(splines2)
library(moments)

getwd()
setwd("C:/Users/USUARIO/Desktop/Análisis estadstico/Curso R/Rstudio/Archivos/14.- Modelos")
entradas <- read_csv("visitantes.csv")

entradas <- entradas %>% filter(anio<2020)
entradas_2 <- entradas %>% group_by(anio, mes)

entradas_3 <- entradas_2 %>% summarise(visitantes=sum(visitantes), .groups = "drop")
entradas_3.1 <- entradas_2 %>% summarise(visitantes=sum(visitantes))
attach(entradas_3.1)
remove(entradas_4)
entradas_4 <- entradas_3 %>% mutate(mes_num=as.factor(mes)) %>% 
  mutate(mes_num=fct_recode(mes_num,
"1"="Enero",
"2"="Febrero",                            
"3"="Marzo",                            
"4"="Abril",                            
"5"="Mayo",                            
"6"="Junio",                            
"7"="Julio",                            
"8"="Agosto",                            
"9"="Septiembre",                            
"10"="Octubre",                            
"11"="Noviembre",
"12"="Diciembre"
)) %>% mutate(mes_num=as.numeric(mes_num)) #Asi lo hice yop/ Todo está bien hasta el as.numeric
#de pronto el error está en que para pasar de caracteres a numeros no es lo indicado
remove(entradas_4)

class(entradas_4.1$mes_num)

                            

entradas_4 <- entradas_3 %>% mutate(mes_num =mes)
entradas_4$mes_num <- gsub("Enero", 1, entradas_4$mes_num)
entradas_4$mes_num <- gsub("Febrero",2, entradas_4$mes_num)
entradas_4$mes_num <- gsub("Marzo", 3, entradas_4$mes_num)
entradas_4$mes_num <- gsub("Abril", 4, entradas_4$mes_num)
entradas_4$mes_num <- gsub("Mayo", 5, entradas_4$mes_num)
entradas_4$mes_num <- gsub("Junio", 6, entradas_4$mes_num)
entradas_4$mes_num <- gsub("Julio", 7, entradas_4$mes_num)
entradas_4$mes_num <- gsub("Agosto", 8, entradas_4$mes_num)
entradas_4$mes_num <- gsub("Septiembre", 9, entradas_4$mes_num)
entradas_4$mes_num <- gsub("Octubre", 10, entradas_4$mes_num)
entradas_4$mes_num <- gsub("Noviembre", 11, entradas_4$mes_num)
entradas_4$mes_num <- gsub("Diciembre", 12, entradas_4$mes_num)

entradas_4 <- entradas_4 %>% mutate(mes_num=as.numeric(mes_num))
class(entradas_4$mes_num)


entradas_4 <- entradas_4 %>% mutate(fecha=make_date(anio, mes_num,1))
  
  
ggplot(entradas_4)+geom_line(aes(x=fecha,y=visitantes), color="purple")
  
ggplot(entradas_4)+geom_line(aes(x=mes,y=visitantes))
  
entradas_5 <- entradas_4 %>% mutate(anios_2=as.factor(anio)) %>% 
  mutate(mes_corto=month(fecha, label=TRUE, abbr = TRUE)) %>% 
  mutate(ln_visitantes=log(visitantes))#abbr = true viene por defecto

class(entradas_5$mes_corto)

ggplot(entradas_5)+ geom_boxplot(mapping = aes(mes_corto, visitantes), 
                                 color="purple", fill="orange", alpha=0.5)
attach(entradas_5)
modelo <- lm(data = entradas_5,ln_visitantes~mes)

summary(modelo)

predict <- entradas_5 %>% add_predictions(modelo) %>% add_residuals(modelo)

hist(predict$resid)
mean(predict$resid)

options(scipen=999)
options(scipen = 0)

ggplot(predict)+geom_point(aes(x=pred, y=resid^2))+
  geom_smooth(aes(x=pred, y=resid^2), color="red", se=FALSE)
  
ggplot(predict, aes(x=pred, y=resid^2))+geom_point()+
  geom_smooth( color="red", se=FALSE)


ggplot(predict, aes(fecha,resid))+
  geom_ref_line(h=0)+geom_line(color="grey50")+
  geom_smooth(se=FALSE, span=.20)
