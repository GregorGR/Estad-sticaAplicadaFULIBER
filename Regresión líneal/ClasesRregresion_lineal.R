install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)
data(cars)
head(cars)
View(cars)
##Diagrama de dispersión
ggplot(cars, aes(x=speed, y=dist)) + 
    geom_point() + 
    geom_smooth(method = lm, se=TRUE)
#Hay una covarianza entre las dos variables 
#al aumentar la otra tambien aumenta
#entonces es un líneal. 

##Mi primer modelo
modelo1 = lm(dist~speed, data=cars)
summary(modelo1)

## Ecuación de regresión estimada
##dist_estimada= Ebeta0 + Ebeta1*speed
dist_estimada <- function(Ebeta0, Ebeta1, speed)
{
    f <- (Ebeta0 + (Ebeta1*speed))
    return(f)
}
dist_estimada(-17.5791, 3.9324, 1)

#Interpretación de los parametros estimados (betas estimados)

###Función diagrama de dispersión regresión lineal simple (revisar formula)
RLsimple <- function(datos, XX, YY) {
    Xi <- select(datos, XX) 
    Yi <- select(datos, YY)
    ggplot(datos, aes(x = Xi, y = Yi)) + 
    geom_point() + 
    geom_smooth(method = lm, se=TRUE) 
}

geiserdat <- read.csv('geiser.csv')
class(geiserdat)
head(geiserdat)
View(geiserdat)

ggplot(geiserdat, aes(x=waiting, y=eruptions)) + 
    geom_point() + 
    geom_smooth(method = lm, se=TRUE) 

##Modelo de taller1
Modelotaller1 = lm(waiting~eruptions, data=geiserdat)
summary(Modelotaller1)

