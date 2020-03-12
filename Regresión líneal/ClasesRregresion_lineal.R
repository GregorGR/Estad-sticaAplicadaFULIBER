###Clase regresi髇 lineal 
install.packages('ggplot2')
library(ggplot2)

###Ejercicio en clase regresi髇 linela simple
data(cars)
head(cars)
View(cars)
##Diagrama de dispersi贸n
ggplot(cars, aes(x=speed, y=dist)) + 
    geom_point() + 
    geom_smooth(method = lm, se=TRUE)
#Hay una covarianza entre las dos variables 
#al aumentar la otra tambien aumenta
#entonces es un l铆neal. 

##Mi primer modelo
modelo1 = lm(dist~speed, data=cars)
summary(modelo1)

## Ecuaci贸n de regresi贸n estimada
##dist_estimada= Ebeta0 + Ebeta1*speed
dist_estimada <- function(Ebeta0, Ebeta1, speed)
{
    f <- (Ebeta0 + (Ebeta1*speed))
    return(f)
}
dist_estimada(-17.5791, 3.9324, 1)

#Interpretaci贸n de los parametros estimados (betas estimados)

#Ejercicio TALLER#1 (REGRESI覰 LINEAL)


###instalaci髇 de paquetes. 
install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)
install.packages('downloader')
library(downloader) 
url <- "https://raw.githubusercontent.com/GregorGR/Estad-sticaAplicadaFULIBER/master/Regresi%C3%B3n%20l%C3%ADneal/geiser.csv?token=AKYIPMGUWRQXJATZZDK3MOC6NKS2Q"
geiserfile <- "geiser.csv" 

geiserdat <- read.csv('geiser.csv')
class(geiserdat)
head(geiserdat)
View(geiserdat)
summary(geiserdat)

###Plot regresi髇 lineal
ggplot(geiserdat, aes(x=waiting, y=eruptions)) + 
    geom_point() + 
    geom_smooth(method = lm, se=TRUE) 

##Modelo de taller1
Modelotaller1 = lm(waiting~eruptions, data=geiserdat)
summary(Modelotaller1)

