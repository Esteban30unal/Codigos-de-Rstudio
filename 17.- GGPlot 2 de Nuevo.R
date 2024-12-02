
# 2.- Previos y Etiquetas -------------------------------------------------


getwd()

library(readxl)
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(lubridate)

enoe <- read_xlsx("mu_enoe.xlsx")
attach(enoe)
g1 <- ggplot(enoe, aes(anios_esc, ingreso_mensual))+
  geom_smooth(mapping = aes(linetype=tipo_empleo))+ 
  geom_point(mapping = aes(color=tipo_empleo))

g1 <- ggplot(enoe, aes(anios_esc, ingreso_mensual, 
                       linetype=tipo_empleo,
                       color=tipo_empleo))+
  geom_smooth()+ 
  geom_point() #Yo lo hice así

g1

ggplot(enoe, aes(anios_esc, ingreso_mensual, 
                       linetype=tipo_empleo,
                       color=tipo_empleo))+
  geom_smooth()+ 
  geom_point() +
  labs(title = "Relación entre los años de escolaridad y el ingreso mensual",
       subtitle = "Según tipo de empleo",
       caption = "Datos de inegi.org.mx",
       x="Años de escolaridad",
       y="Ingreso mensual $",
       color= "Tipo de empleo",
       linetype= "Tipo de empleo",
       tag= "Gráfica 1"
    
  )

etiquetax <- quote(sum(años))
etiquetay <- paste("Ingreso Mensual Máximo Registrado: $", 
                   max(enoe$ingreso_mensual), sep="\n")
ggplot(enoe, aes(anios_esc, ingreso_mensual, 
                       linetype=tipo_empleo,
                       color=tipo_empleo))+
  geom_smooth()+ 
  geom_point() +
  labs(title = "Relación entre los años de escolaridad y el ingreso mensual",
       subtitle = "Según tipo de empleo",
       caption = "Datos de inegi.org.mx",
       x= etiquetax,
       y= etiquetay,
       color= "Tipo de empleo",
       linetype= "Tipo de empleo",
       tag= "Gráfica 1"
    
  )


# 3.- Anotaciones ---------------------------------------------------------

tipo <- enoe %>% 
  group_by(tipo_empleo, niv_edu, anios_esc) %>% 
  summarise(max_na=max(ingreso_mensual)) %>% 
  group_by(tipo_empleo, niv_edu) %>% 
  filter(max_na==max(max_na))
names(tipo)[4]= "ingreso_mensual"

tipo
attach(enoe)
ggplot(enoe, aes(anios_esc, ingreso_mensual))+
  geom_point(aes(color=niv_edu))+
  geom_text(aes(label = tipo_empleo), data= tipo)

ggplot(enoe, aes(anios_esc, ingreso_mensual))+
  geom_point(aes(color=niv_edu))+
  geom_label(aes(label = tipo_empleo), data= tipo, alpha=0.3, size=3)

ggplot(enoe, aes(anios_esc, ingreso_mensual, color= niv_edu))+
  geom_label_repel(mapping = aes(label = niv_edu), data= tipo, size=3, 
                            segment.color= NA)+
  geom_point()

ggplot(enoe , aes(anios_esc,ingreso_mensual, color=niv_edu))+
  ggrepel::geom_label_repel(aes(label = niv_edu), data = tipo, size=3,
                            segment.color=NA)+
  geom_point()


replace_null
conflicts()
sessionInfo()

sum(is.na(tipo$anios_esc))
sum(is.na(tipo$ingreso_mensual))
sum(is.na(tipo$niv_edu))
 
mayor_ing <- enoe %>% 
  filter(ingreso_mensual==max(ingreso_mensual)) %>% 
  select(estado, niv_edu, ingreso_mensual, tipo_empleo, anios_esc) %>% 
  mutate(texto="Mayor ingreso")



ggplot(enoe, aes(anios_esc, ingreso_mensual))+
  geom_point(aes(color=niv_edu)) +
  geom_text(aes(label= texto), data=mayor_ing, size=3, 
          color="black", hjust="center", vjust="bottom")+ 
            geom_point(aes(anios_esc, ingreso_mensual),
                       data=mayor_ing, color="purple")
          

ggplot(enoe, aes(anios_esc, ingreso_mensual))+
  geom_point(aes(color=niv_edu)) +
  geom_text(aes(label= texto), data=mayor_ing, size=3, 
          color="black", hjust="center", vjust="bottom")+ 
            geom_point(aes(anios_esc, ingreso_mensual),
                       data=mayor_ing, color="purple")+
  geom_hline(yintercept = mean(enoe$ingreso_mensual), color="red")+
  geom_vline(xintercept = mean(enoe$anios_esc), color="red")





ggplot(enoe, aes(anios_esc, ingreso_mensual))+
  geom_point(aes(color=niv_edu)) +
  geom_text(aes(label= texto), data=mayor_ing, size=3, 
          color="black", hjust="center", vjust="bottom")+ 
            geom_point(aes(anios_esc, ingreso_mensual),
                       data=mayor_ing, color="purple")+
  geom_hline(yintercept = q1 , color="black")+
  geom_hline(yintercept = q2 , color="black")+
  geom_hline(yintercept = mean(enoe$ingreso_mensual), color= "Red")
attach(enoe)
qnorm(c(0.025, 0.975))
quantile(enoe$ingreso_mensual, c(0.025, 0.975), na.rm = TRUE)
quantile(enoe$anios_esc, c(0.025, 0.975), na.rm = TRUE)
q1<- quantile(enoe$ingreso_mensual, c(0.025), na.rm = TRUE)
q2<- quantile(enoe$ingreso_mensual, c(0.975), na.rm = TRUE)

summary(enoe$ingreso_mensual)


# 4.- Escalas -------------------------------------------------------------

(g5 <- {
  ggplot(enoe, aes(anios_esc, ingreso_mensual))+
  geom_point(aes(color=niv_edu)) +
  geom_text(aes(label= texto), data=mayor_ing, size=2.5, 
          color="black", hjust=-0.1, vjust="bottom")+ 
            geom_point(aes(anios_esc, ingreso_mensual),
                       data=mayor_ing, color="purple")+
  geom_hline(yintercept = mean(enoe$ingreso_mensual), color="red")+
  geom_vline(xintercept = mean(enoe$anios_esc), color="red")+
  labs(
    title = "Relación entre los años de escolaridad y el ingreso",
    subtitle = "Según tipo de empleo",
    caption = "Datos de inegi.org.mx",
    x= "Años de escolaridad",
    y= "Ingreso Mensual $",
    color="Tipo de Empleo",
    linetype="Tipo de Empleo")+
  geom_hline(yintercept = q1 , color="black")+
  geom_hline(yintercept = q2 , color="black")+
  scale_y_continuous(breaks = seq(0, 80000, by=5000))+
  scale_x_continuous(breaks = seq(0,25, by=1))
 })


(g5 <- {
  ggplot(enoe, aes(anios_esc, ingreso_mensual))+
  geom_point(aes(color=niv_edu)) +
  geom_text(aes(label= texto), data=mayor_ing, size=2.5, 
          color="black", hjust=-0.1, vjust="bottom")+ 
            geom_point(aes(anios_esc, ingreso_mensual),
                       data=mayor_ing, color="purple")+
  geom_hline(yintercept = mean(enoe$ingreso_mensual), color="red")+
  geom_vline(xintercept = mean(enoe$anios_esc), color="red")+
  labs(
    title = "Relación entre los años de escolaridad y el ingreso",
    subtitle = "Según tipo de empleo",
    caption = "Datos de inegi.org.mx",
    x= "Años de escolaridad",
    y= "Ingreso Mensual $",
    color="Tipo de Empleo",
    linetype="Tipo de Empleo")+
  geom_hline(yintercept = q1 , color="black")+
  geom_hline(yintercept = q2 , color="black")+
  scale_y_continuous(breaks = seq(0, 80000, by=5000))+
  scale_x_continuous(breaks = seq(0,26, by=1), labels = et_anios)
 })
et_anios <- paste(seq(0,26, by=1),"A")


# 5.- Leyendas ------------------------------------------------------------


g5+
  theme(legend.position = "left")

g5+
  theme(legend.position = "bottom")+
  guides(color=guide_legend(nrow=2, override.aes = list(size=3)))

# 6.- Colores -------------------------------------------------------------

g5+
  scale_color_brewer(palette = "BrBG")

g5+
  scale_color_brewer(palette = "BuPu")
g5+
  scale_color_brewer(palette = "Dark2")
attach(enoe)
({g6 <-  ggplot(data = enoe)+ 
  geom_point(mapping = aes(x=anios_esc, y=ingreso_mensual,
                           color=hrsocup
                           ))+
  scale_colour_gradient(low="white", high = "red")})



#  7.- Temas textos y Colores ---------------------------------------------


g5+ggplot(enoe, aes(anios_esc, ingreso_mensual))+
  geom_bar(aes(fill = ingreso_mensual), stat = "identity")+
  scale_fill_gradient(low = "yellow", high = "red", na.value= NA)+
scale_y_continuous(breaks = seq(0, 30000000, by=1000000))+
  scale_x_continuous(breaks = seq(0,26, by=1))+
  theme_dark()+theme(legend.position = "NULL ")

(g5 <- {
  ggplot(enoe, aes(anios_esc, ingreso_mensual))+
    geom_point(aes(color=niv_edu)) +
    geom_text(aes(label= texto), data=mayor_ing, size=2.5, 
              color="black", hjust=-0.1, vjust="bottom")+ 
    geom_point(aes(anios_esc, ingreso_mensual),
               data=mayor_ing, color="purple")+
    geom_hline(yintercept = mean(enoe$ingreso_mensual), color="red")+
    geom_vline(xintercept = mean(enoe$anios_esc), color="red")+
    labs(
      title = "Relación entre los años de escolaridad y el ingreso",
      subtitle = "Según tipo de empleo",
      caption = "Datos de inegi.org.mx",
      x= "Años de escolaridad",
      y= "Ingreso Mensual $",
      color="Tipo de Empleo",
      linetype="Tipo de Empleo")+
    theme_dark()+theme(legend.position = "NULL")+
    scale_y_continuous(breaks = seq(0, 800000, by=5000))+
    scale_x_continuous(breaks = seq(0,26, by=1))
  })

titulo_grafica <- element_text(
  family = "Arial Narrow", face = "bold", color = "blue", size= 10)
subtitulo_grafica <- element_text(
  family = "Arial Narrow", color = "red", size= 10)

g5+theme(plot.title = titulo_grafica, plot.subtitle = subtitulo_grafica)

texto_ejes <- element_text(
  family = "Comic Sans MS", face = "bold", color = "brown", size= 8)
texto_leyenda <- element_text(
  family = "Comic Sans MS", face = "bold",color = "purple", size= 10)

g5+ theme_classic()+theme(plot.title = titulo_grafica, 
         plot.subtitle = subtitulo_grafica,
         axis.title.x = texto_ejes,
         axis.text.y = texto_ejes,
         legend.text = texto_ejes, 
         legend.title = texto_leyenda)
 
g5+ theme_classic()+theme(plot.title = titulo_grafica, 
         plot.subtitle = subtitulo_grafica,
         axis.title.x = texto_ejes,
         axis.text.y = texto_ejes,
         legend.position = "NULL")



