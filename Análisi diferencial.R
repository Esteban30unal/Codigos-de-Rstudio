options(repos = c(CRAN = "https://cran.rstudio.com/"))
chooseCRANmirror()
install.packages("writexl")
install.packages("corrplot")
install.packages("ggcorrplot")
library(tidyverse)
library(purrr)
library(readxl)
library(writexl)
library(corrplot)
library(ggcorrplot)
library(psych)
# Mostrar las primeras filas del data.frame
head(datos)
file.choose()
datos <- read_excel("Trabajo.xlsx",
                    sheet = "Hoja1")
datos <- datos %>% select(1:241) %>% na.omit()



# Establecer un número fijo de niveles para las variables ordinales
niveles_ordinales <- c("En total desacuerdo", "En desacuerdo", "Neutral","De acuerdo","Totalmente de acuerdo")

unique(datos$item_1)

# Especificar las columnas a invertir (por ejemplo, las primeras 80)
variables_invertir <- c("item_1","item_4","item_7","item_8","item_10","item_11","item_14","item_17","item_18","item_20","item_21","item_24","item_27","item_28","item_30","item_32","item_33","item_35","item_36","item_39",
                        "item_42","item_43","item_45","item_46","item_49","item_52","item_53","item_55","item_56","item_59","item_61","item_64","item_67","item_68","item_70","item_71","item_74","item_77","item_78",
                        "item_80","item_81","item_84","item_87","item_88","item_90","item_92","item_93","item_95","item_96","item_99","item_102","item_103","item_105","item_106","item_109","item_112",
                        "item_113","item_115","item_116","item_119","item_121","item_124","item_127","item_128","item_130","item_134","item_137","item_138","item_140","item_141",
                        "item_144","item_147","item_148","item_150","item_153","item_155","item_156","item_159","item_162","item_163","item_166","item_169","item_173","item_175",
                        "item_176","item_181","item_183","item_187","item_189","item_190","item_198","item_199","item_205","item_206","item_207","item_208","item_213","item_219",
                        "item_220","item_222","item_228","item_229","item_231","item_234","item_236","item_238"
)

variables_invertir_2 <- c(1,4,7,8,10,11,14,17,18,20,21,24,27,28,30,32,33,35,36,39,
                          42,43,45,46,49,52,53,55,56,59,61,64,67,68,70,71,74,77,78,
                          80,81,84,87,88,90,92,93,95,96,99,102,103,105,106,109,112,
                          113,115,116,119,121,124,127,128,130,134,137,138,140,141,
                          144,147,148,150,153,155,156,159,162,163,166,169,173,175,
                          176,181,183,187,189,190,198,199,205,206,207,208,213,219,
                          220,222,228,229,231,234,236,238)
# Definir los niveles de las variables ordinales
niveles <- niveles_ordinales

# Crear la función para codificar
codificar_variable <- function(variable, niveles, invertir = FALSE) {
  if (invertir) {
    return(as.numeric(factor(variable, levels = rev(niveles), ordered = TRUE)))
  } else {
    return(as.numeric(factor(variable, levels = niveles, ordered = TRUE)))
  }
}

# Aplicar la codificación a las 240 columnas
library(dplyr)

datos_codificados <- datos %>%
  mutate(across(.cols = everything(), 
                .fns = ~ codificar_variable(., 
                                            niveles = niveles, 
                                            invertir = cur_column() %in% variables_invertir)))


datos_codificados_2 <- datos %>%
  mutate(across(.cols = colnames(datos[, c(1:240)]), 
                .fns = ~ codificar_variable(., 
                                            niveles = niveles, 
                                            invertir = cur_column() %in% colnames(datos)[variables_invertir_2])))

# Mostrar las primeras filas del data.frame codificado
head(datos_codificados)




getwd()

getwd()
datos <- read_excel("C:\\Users\\USUARIO\\Desktop\\Análisis estadstico\\Análisis diferencial\\trabajo_codificado.xlsx")

5+4+4+4+5+5+4+4
hist(datos_codificados_3$N)
hist(datos_codificados_3$N1)
hist(datos_codificados_3$N2)
hist(datos_codificados_3$N3)
hist(datos_codificados_3$N4)
hist(datos_codificados_3$N5)
hist(datos_codificados_3$N6)
summary(datos_codificados_3$O)
ks.test(datos_codificados_3$N,pnorm, mean(datos_codificados_3$N),sd(datos_codificados_3$N))
ks.test(datos_codificados_3$N1,pnorm, mean(datos_codificados_3$N1),sd(datos_codificados_3$N1))
ks.test(datos_codificados_3$N2,pnorm, mean(datos_codificados_3$N2),sd(datos_codificados_3$N2))
ks.test(datos_codificados_3$N3,pnorm, mean(datos_codificados_3$N3),sd(datos_codificados_3$N3))
ks.test(datos_codificados_3$N4,pnorm, mean(datos_codificados_3$N4),sd(datos_codificados_3$N4))
ks.test(datos_codificados_3$N5,pnorm, mean(datos_codificados_3$N5),sd(datos_codificados_3$N5))
ks.test(datos_codificados_3$N6,pnorm, mean(datos_codificados_3$N6),sd(datos_codificados_3$N6))
ks.test(f,pnorm, mean(f),sd(f))

f <- rnorm(10000, 100,15)

class(datos_codificados_2$id)
datos_codificados_3 <- datos_codificados_2 %>% 
  mutate(N1=rowSums(datos_codificados_2[, c(1,31,61,91,121,151,181,211)]), 
         N2=rowSums(datos_codificados_2[, c(6,36,66,96,126,156,186,216)]),
         N3=rowSums(datos_codificados_2[, c(11,41,71,101,131,161,191,221)]),
         N4=rowSums(datos_codificados_2[, c(16,46,76,96,136,166,196,226)]),
         N5=rowSums(datos_codificados_2[, c(21,51,81,111,141,171,201,231)]),
         N6=rowSums(datos_codificados_2[, c(26,56,86,116,146,176,206,236)]),
         N=(N1+N2+N3+N4+N5+N6),
         
         E1=rowSums(datos_codificados_2[, c(2,32,62,92,122,152,182,212)]), 
         E2=rowSums(datos_codificados_2[, c(7,37,67,97,127,157,187,217)]),
         E3=rowSums(datos_codificados_2[, c(12,42,72,102,132,162,192,222)]),
         E4=rowSums(datos_codificados_2[, c(17,47,77,107,137,167,197,227)]),
         E5=rowSums(datos_codificados_2[, c(22,52,82,112,142,172,202,232)]),
         E6=rowSums(datos_codificados_2[, c(27,57,87,117,147,177,107,227)]),
         E=(E1+E2+E3+E4+E5+E6),
         
         O1=rowSums(datos_codificados_2[, c(3,33,63,93,123,153,183,213)]), 
         O2=rowSums(datos_codificados_2[, c(8,38,68,98,128,158,188,218)]),
         O3=rowSums(datos_codificados_2[, c(13,43,73,103,133,163,193,223)]),
         O4=rowSums(datos_codificados_2[, c(18,48,78,108,138,168,198,228)]),
         O5=rowSums(datos_codificados_2[, c(23,53,83,113,143,173,203,233)]),
         O6=rowSums(datos_codificados_2[, c(28,58,88,118,148,178,108,228)]),
         O=(O1+O2+O3+O4+O5+O6),
         
         A1=rowSums(datos_codificados_2[, c(4,34,64,94,124,154,184,214)]), 
         A2=rowSums(datos_codificados_2[, c(9,39,69,99,129,159,189,219)]),
         A3=rowSums(datos_codificados_2[, c(14,44,74,104,134,164,194,224)]),
         A4=rowSums(datos_codificados_2[, c(19,49,79,109,139,169,199,229)]),
         A5=rowSums(datos_codificados_2[, c(24,54,84,114,144,174,204,234)]),
         A6=rowSums(datos_codificados_2[, c(29,59,89,119,149,179,109,229)]),
         A=(A1+A2+A3+A4+A5+A6),
         
         C1=rowSums(datos_codificados_2[, c(5, 35, 65, 95,125,155,185,215)]), 
         C2=rowSums(datos_codificados_2[, c(10,40,70,100,130,160,190,220)]),
         C3=rowSums(datos_codificados_2[, c(15,45,75,105,135,165,195,225)]),
         C4=rowSums(datos_codificados_2[, c(20,50,80,110,140,170,200,230)]),
         C5=rowSums(datos_codificados_2[, c(25,55,85,115,145,175,205,235)]),
         C6=rowSums(datos_codificados_2[, c(30,60,90,120,150,180,210,240)]),
         C=(C1+C2+C3+C4+C5+C6)
  )



archivo_excel <- "trabajo_codificado_Adriana.xlsx"
write_xlsx(datos_codificados_3, path = archivo_excel)


archivo_excel_2 <- "trabajo_codificado_2.xlsx"
write_xlsx(datos_codificados, path = archivo_excel_2)


datos_codificados_3 <- datos_codificados_3 %>%
  relocate(id, .after = 276)
analisis <- datos_codificados_3 %>% 
  select(241:276)

datos_autoeficacia <- read_excel("Trabajo.xlsx",
                                 sheet = "Hoja2")#No usar
datos_autoeficacia <- datos_autoeficacia %>% rename(puntuacion_autoeficacia=`Puntuación directa`)#No usar 



autoeficacia <- read_excel("Trabajo.xlsx",
                           sheet = "Hoja2", na="NA") %>% na.omit() %>% 
  select(c(11,12)) %>% rename(Puntuación_directa=`Puntuación directa` )





analisis_final <- left_join(analisis, autoeficacia, by="id")


analisis_final <- analisis_final %>% 
  rename(puntuación_autoeficacia=Puntuación_directa)#No usar

analisis_final <- analisis_final %>%
  relocate(id, .after = 37)#no usar

analisis_final$id <- as.factor(analisis_final$id)

analisis_final_z_eliminadas <- anti_join(analisis_final_z, analisis_final_z_sin)
view(analisis_final_z_eliminadas)
columnas_atipicas <- colnames(analisis_final_z_eliminadas %>%
                                summarise(across(where(is.numeric), ~ any(abs(.) > 3)))) 
analisis_final_z_sin <- analisis_final_z_sin %>% 
  rename(Autoeficacia=Puntuacion_directa)


# Omitir una variable llamada 'var_a' al seleccionar variables numéricas
normalidad <- analisis_final_z %>%
  select(where(is.numeric))

# Crear una función para aplicar Kolmogorov-Smirnov
ks_test_normal <- function(x) {
  ks.test(x, "pnorm", mean=mean(x), sd=sd(x))$p.value
} 


# Aplicar la prueba a todas las variables numéricas
ks_results <- normalidad %>%
  summarise(across(everything(), ks_test_normal))

# Mostrar los resultados
ks_results

fila <- ks_results[1, ]
resultado <- data.frame(Columna = names(fila), Valor = unlist(fila))#No usar
print(resultado)

library(knitr)
fila <- ks_results[1, ]
kable(as.data.frame(t(fila)))#No usar





hist(analisis_final_z$O)

analisis_final_z <- analisis_final %>%
  mutate(across(-id, ~ as.numeric(scale(.))))

colnames(analisis_final_t)

analisis_final_t <- analisis_final %>%
  mutate(across(-id, ~ as.numeric(50 + 10*scale(.))))

colnames(analisis_final_t)

analisis_final_t <- analisis_final_t %>% 
  rename(Autoeficacia=Puntuación_directa)

colnames(analisis_final_t)

mode(analisis_final_t$Autoeficacia)



analisis_final_z$id <- as.character(analisis_final_z$id)

analisis_final_z_sin <- analisis_final_z %>% 
  filter(if_all(where(is.numeric), ~ . > -3 & . < 3))

analisis_final_z_eliminadas <- anti_join(analisis_final_z, analisis_final_z_sin)

analisis_final_z_eliminadas<-analisis_final_z_eliminadas %>% 
  select(where(~ any(. < -3 | . > 3)))


columnas_atipicas <- colnames(analisis_final_z_eliminadas %>%
                                summarise(across(where(is.numeric), ~ any(abs(.) > 3)))) 
view(columnas_atipicas)
analisis_final_z_sin <- analisis_final_z_sin %>% 
  rename(Autoeficacia=Puntuacion_directa)


analisis_final_z_eliminadas <- analisis_final_z_eliminadas %>%
  select(id, everything()) %>%
  select(id, all_of(names(analisis_final_z_eliminadas)))#Revisar si quedó bien la variable id

analisis_final_z_sin <- analisis_final_z_sin %>% 
  rename(Autoeficacia=Puntuación_directa)
cor_pearson <- cor(analisis_final_t %>% select(N, E, O, A, C,Autoeficacia), 
                   use = "complete.obs", method = "pearson")
cor_pearson <- cor(analisis_final_z_sin %>% select(N, E, O, A, C,Autoeficacia), 
                   use = "complete.obs", method = "pearson")
cor_pearson



cor_spearman  <- cor(analisis_final_t %>% select(N, E, O, A, C,Autoeficacia), 
                     use = "complete.obs", method = "spearman")
cor_spearman
view(cor_spearman)
install.packages("apaTables")
library(apaTables)
cor_spearman  <- cor(analisis_final_z %>% select(N, E, O, A, C,Autoeficacia), 
                     use = "complete.obs", method = "spearman")
cor_spearman_<- as.data.frame(cor_spearman)
apa.cor.table(cor_spearman %>% select(N, E, O, A, C, Autoeficacia),
              filename = "tabla_correlacion_apa_con_significancia.doc", 
              show.conf.interval = FALSE,
              table.number=1,
              cor.type = "spearman"
)

apa.cor.table(analisis_final_z %>% select(N, E, O, A, C, Autoeficacia),
              filename = "tabla_correlacion_apa.doc", 
              show.conf.interval = FALSE,
              table.number=1
)
getwd()


library(knitr)
kable(cor_spearman, format = "markdown", caption = "Correlaciones de Spearman entre N, E, O, A, C, y Autoeficacia")


corrplot(cor_pearson, method = "circle",tl.cex = 0.6, cl.cex = 0.8, mar = c(0, 0, 0, 0))

corrplot(cor_spearman, method = "circle", tl.cex = 0.6, cl.cex = 0.8, mar = c(0, 0, 0, 0))


# Visualizar la matriz de correlación de Pearson

gp <- ggcorrplot(cor_pearson, lab = TRUE, title = "Pearson")
gp

# Visualizar la matriz de correlación de Spearman
p.mat <- cor_pmat(analisis_final_z %>% select(N, E, O, A, C, Autoeficacia))
ggcorrplot(cor_spearman, 
           lab = TRUE,
           lab_size = 5,
           title = "Spearman",
           method = "square",
           type = "upper",
           sig.level = 0.05,
           insig = "blank",
           colors = c("blue", "white", "red"),
           outline.color = "black",
           ggtheme = ggplot2::theme_void(),
           show.diag = FALSE,
           p.mat=p.mat,
           legend.title = "Correlación Spearman",
           
)

ggcorrplot(cor_spearman, 
           lab = TRUE,
           lab_size = 5,
           title = "Spearman",
           method = "square",
           type = "upper",
           sig.level = 0.05,
           insig = "blank",
           colors = c("blue", "white", "red"),
           outline.color = "black",
           ggtheme = ggplot2::theme_void(),
           show.diag = FALSE,
           legend.title = "Correlación Spearman",
           
)


gs


suppressWarnings(ks.test(analisis_final_z$N,pnorm, mean(analisis_final_z$N),sd(analisis_final_z$N)))
suppressWarnings(ks.test(analisis_final_z$O,pnorm, mean(analisis_final_z$O),sd(analisis_final_z$O)))
suppressWarnings(ks.test(analisis_final_z$E,pnorm, mean(analisis_final_z$E),sd(analisis_final_z$E)))
suppressWarnings(ks.test(analisis_final_z$A,pnorm, mean(analisis_final_z$A),sd(analisis_final_z$A)))
suppressWarnings(ks.test(analisis_final_z$C,pnorm, mean(analisis_final_z$C),sd(analisis_final_z$C)))
suppressWarnings(ks.test(analisis_final_z$Autoeficacia,pnorm, mean(analisis_final_z$Autoeficacia),sd(analisis_final_z$Autoeficacia)))



hist(analisis_final_z$O)



plot(analisis_final_z_sin$N,analisis_final_z_sin$Autoeficacia,
     pch=19,
     col="purple",
)

pairs.panels(analisis_final_z %>% select(N, E, O, A, C,Autoeficacia), 
             method = "spearman",
             pch=20,
             hist.col="purple",
             rug=FALSE,
             smooth=TRUE,
             density=TRUE,
             ellipses=FALSE,
             lm=TRUE,
             stars = TRUE,
             cex.cor = 1.8,
             cex.axis = 1.5,
             cex.labels = 1.5,
             bg = "white"
)


# Establecer el archivo PDF de salida
pdf("grafica_pairs_panels.pdf", width = 8, height = 8)
colnames(analisis_final_z_sin)
# Crear la gráfica
pairs.panels(analisis_final_z %>% select(N, E, O, A, C,Autoeficacia), 
             method = "spearman",
             pch=20,
             hist.col="purple",
             rug=FALSE,
             smooth=TRUE,
             density=TRUE,
             ellipses=FALSE,
             lm=TRUE,
             stars = TRUE,
             cex.cor = 1.2,
             cex.axis = 1,
             cex.labels = 1,
             bg = "white"
)

# Cerrar el dispositivo gráfico
dev.off()
getwd()
pairs.panels(analisis_final_z %>% select(N, N1, N2, N3, N4, N5, N6,Autoeficacia), 
             method = "spearman",
             pch=20,
             hist.col="purple",
             rug=FALSE,
             smooth=TRUE,
             density=TRUE,
             ellipses=FALSE,
             lm=TRUE
)
colnames(analisis_final_z_sin)
boxplot(analisis_final_z %>% select(N, E, O, A, C,Autoeficacia), main = "Boxplot de varias variables")



sociodemograficos <- read_excel("Trabajo.xlsx",
                                sheet = "Hoja3", na="NA") %>% na.omit()
sociodemograficos$id <- as.character(sociodemograficos$id)
sociodemograficos <- left_join(sociodemograficos, analisis_final_z, by="id") %>% 
  na.omit()
summary(sociodemograficos$Edad)
sd(sociodemograficos$Edad)
summarise(sociodemograficos$Género)

conteo_genero <- sociodemograficos %>%
  count(Género)
print(conteo_genero)
rango_edad <- max(sociodemograficos$Edad, na.rm = TRUE) - min(sociodemograficos$Edad, na.rm = TRUE)
print(rango_edad)

max(sociodemograficos$Edad)
min(sociodemograficos$Edad)

citation("ggcorrplot")
sociodemograficos$Estrato_socioeconómico <- as.character(sociodemograficos$Estrato_socioeconómico)
sociodemograficos$Estrato_socioeconómico <- as.numeric(sociodemograficos$Estrato_socioeconómico)
conteo_estrato <- sociodemograficos %>%
  count(Estrato_socioeconómico)
print(conteo_estrato)
hist(sociodemograficos$Estrato_socioeconómico,
     xlab="Estrato",
     main = "Histograma Estrato Socioeconómico",
     ylim = c(0, 50),
     
)

cor_spearman  <- cor(analisis_final_z %>% select(N, E, O, A, C,Autoeficacia), 
                     use = "complete.obs", method = "spearman")







library(ggplot2)
library(gridExtra)

# Primer gráfico con ggplot2
g1 <- ggplot(analisis_final_z, aes(x = analisis_final_z$Autoeficacia, y = analisis_final_z$N)) +
  geom_point() +
  ggtitle("Autoeficacia vs N")+
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  geom_point(color = "purple")+
  labs(x = "", y = "")

# Segundo gráfico
g2 <- ggplot(analisis_final_z, aes(x = analisis_final_z$Autoeficacia, y = analisis_final_z$E)) +
  geom_point() +
  ggtitle("Autoeficacia vs E")+
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  geom_point(color = "purple")+
  labs(x = "", y = "")

# Tercer gráfico
g3 <- ggplot(analisis_final_z, aes(x = analisis_final_z$Autoeficacia, y = analisis_final_z$C)) +
  geom_point() +
  ggtitle("Autoeficacia vs C")+
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  geom_point(color = "purple")+
  labs(x = "", y = "")

# Combinar los tres gráficos en una fila
grid.arrange(g1, g2, g3, ncol = 3)

maria <- analisis_final_z %>% filter(id==76)

png("grafica_gp.png", width = 1600, height = 1200, res = 300) # Ajusta el tamaño y resolución según sea necesario
print(gp) # Reemplaza 'plot' con tu objeto de gráfico
dev.off() # Cierra el dispositivo gráfico

png("grafica_gs.png", width = 1600, height = 1200, res = 300) # Ajusta el tamaño y resolución según sea necesario
print(gs) # Reemplaza 'plot' con tu objeto de gráfico
dev.off() # Cierra el dispositivo gráfico