library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
getwd() 
file.choose()
setwd("C:/Users/USUARIO/Desktop/Análisis estadstico/Curso R/Rstudio")
enoe<-read_excel("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Curso R\\Rstudio\\Archivos\\3.- GGPlot 2\\mu_enoe.xlsx")
attach(enoe)
ggplot(data = enoe) +
  geom_point(mapping = aes(x =anios_esc, y =ingreso_mensual, color=sex
  )) +
  geom_smooth(mapping=aes(x=anios_esc, y = ingreso_mensual)) 

ggplot(data = enoe) +
  geom_point(mapping = aes(x =anios_esc, y =ingreso_mensual, color= tipo_empleo
  )) +
  geom_smooth(mapping=aes(x=anios_esc, y = ingreso_mensual)) +
  facet_wrap(~sex, nrow=1, ncol=2)

ggplot(data = enoe) +
  geom_point(mapping = aes(x =anios_esc, y =ingreso_mensual
  ), color="purple") +
  geom_smooth(mapping=aes(x=anios_esc, y = ingreso_mensual)) +
  facet_grid(tipo_empleo~sex)

ggplot(datos= enoe,mapping = aes (x= anios_esc, y = ingreso_mensual)) +
  geom_point(mapping = aes(color= tipo_empleo)) +
  geom_smooth(mapping = aes(linetype= tipo_empleo))


ggplot(data = enoe) +
  geom_point(mapping = aes(x =anios_esc, y =ingreso_mensual, color= tipo_empleo
  )) +
  geom_smooth(mapping=aes(x=anios_esc, y = ingreso_mensual)) +
  facet_wrap(~sex, nrow=1, ncol=2) 

ggplot(data=enoe) + geom_point(mapping = aes(x=anios_esc, y= ingreso_mensual), color="purple") +
facet_grid(tipo_empleo~sex) + geom_smooth(mapping = aes(x= anios_esc, y= ingreso_mensual), color="red")

ggplot(data=enoe)+ geom_point(mapping = aes(x=anios_esc, y=ingreso_mensual), color= "purple") + 
  geom_smooth(mapping = aes(x=anios_esc, y=ingreso_mensual,linetype= sex)) + 
  facet_grid(~tipo_empleo)

ggplot(data=enoe, mapping= aes(x= anios_esc, y= ingreso_mensual))+ 
  geom_point(mapping = aes(color= tipo_empleo), show.legend = FALSE)+ 
  geom_smooth(mapping = aes(linetype= sex), show.legend = FALSE) + 
  facet_grid(~tipo_empleo)


ggplot(data=enoe, mapping= aes(x= anios_esc, y= ingreso_mensual))+ 
  geom_point(mapping = aes(color=niv_edu),show.legend = FALSE)+
  facet_grid(tipo_empleo~sex)


ggplot(data=enoe, mapping = aes(x =anios_esc, y =ingreso_mensual))+
  geom_point(mapping = aes(color=niv_edu), show.legend = FALSE)+
  geom_smooth(data=filter(enoe, estado=="Jalisco"), se=FALSE)


ggplot(data=enoe)+
  geom_bar(mapping = aes(x=sex))
  
ggplot(data=enoe)+
  stat_count(mapping = aes(x=sex))

ggplot(data=enoe)+
  geom_bar(mapping = aes(x=sex, y=..prop..,group=1))

?geom_bar
?geom_smooth


ggplot(datos= enoe)+
  stat_summary(
    mapping = aes(x=sex, y= ingreso_mensual),
    fun.min=min,
    fun.max=max,
    fun=median )

ggplot(data=enoe) +
  geom_bar(mapping = aes(x=sex, fill=sex))

ggplot(data=enoe) +
  geom_bar(mapping = aes(x=sex, fill=niv_edu))

ggplot(data=enoe) +
  geom_bar(mapping = aes(x=sex, fill=niv_edu), alpha=1/5, position = "identity")

ggplot(data=enoe) +
  geom_bar(mapping = aes(x=sex, color=niv_edu), fill=NA, position = "identity")

ggplot(data=enoe) +
  geom_bar(mapping = aes(x=sex, fill=niv_edu), position = "fill")

ggplot(data=enoe) +
  geom_bar(mapping = aes(x=factor(1), fill=niv_edu), position = "fill")


ggplot(data=enoe) +
  geom_bar(mapping = aes(x=sex, fill=niv_edu), position = "dodge") +
  labs(title = "observaiones por sexo y nivel educativo", x="sexo", 
       y= "observaciones")
?geom_bar()

 ggplot(data= enoe)+
   geom_point( mapping = aes(x= anios_esc, y = ingreso_mensual, color=sex), 
               position = "jitter")

 ggplot(data=enoe, mapping = aes(x=niv_edu, y= ingreso_mensual, color=sex))+
   geom_boxplot() + facet_grid(~sex)
 
 ggplot(data=enoe, mapping = aes(x=niv_edu, y= ingreso_mensual))+
   geom_boxplot(color="purple") + coord_flip()
 
 ggplot(data=enoe, mapping = aes(x=factor(1), fill=niv_edu )) + 
   geom_bar(position = "fill") +
   coord_polar(theta = "y") + 
   labs(x="", y="")
 
 










