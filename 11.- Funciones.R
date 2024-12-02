#  1.- Intro
#Me siento orgulloso de ti. Vas lento pero estás avanzando 
#Introducción
#En este capítulo trabajaremos con dos opciones que permitirán hacer mas 
#eficiente la construcción de código. Profundizaremos en el uso de la familia de 
#operadores del tipo pipe y aprenderemos a construir nuestras propias funciones. 
#Los elementos que se trabajan en este capítulo harán que el proceso de programación 
#sea mas elegante, sencillo e intuitivo.

#No sólo para nosotros, si no además para aquellos con los que tengamos que 
#compartir el trabajo. Usaremos una base de datos, que contiene únicamente un 
#conjunto de valores aleatorios, los cuales no tienen ninguna interpretación específica. 
#Esto no es una limitante, pues las técnicas que aprendamos podrán ser 
#replicadas a cualquier otra base de datos, de las diversas que hemos presentado en este curso.
#2.- Funciones


area<-function(x,y){x*y}


area(10,5)

#3.- Condicionales

signo<- function(x){if(x>0){print("Positivo")} else {print("Negativo")}}

signo(9)
signo(-4)
signo(0)

signo<- function(x){if(x>0){print("Positivo")
  } else if(x==0){print("cero")} else {print("Negativo")}}


signo(0)

operacion<-function(x,y,opera){
  switch (opera,
    "suma" = x+y,
    "resta"=x-y,
    "multiplica"=x*y,
    "divide"=x/y, stop("operación desconocida")
  )
}

operacion(4,8,"resta")
operacion(4,8,"papas")
#4.- Argumentos de una Función
mean()
veces<- function(x, num=2){num*x}
veces(4)
veces(4,num=5)

punto2<- function(x,y){if (length(x)!=length(y)){stop("x, y deben ser del mismo tamaño")}
  else{x*y}
}

a<-1:3
b<-1:6
punto2(a,b)
a<-1:6
punto2(a,b)


#5.- Valores de Regreso

complejo<- function(x,y){if (length(x)==0||length(y)==0){return(0)}else{x+y}
  }

complejo(NULL, 6)

