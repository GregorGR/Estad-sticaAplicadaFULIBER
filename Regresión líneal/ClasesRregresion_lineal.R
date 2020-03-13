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
install.packages('ggthemes')
library(ggthemes)
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

###Funci髇 para Graficar y para sacar modelo
RLsimple_f <- function(datos, xi, yi) {
    require(ggplot2)
    require(ggthemes)
    p <- ggplot(datos, aes(x = get(xi),
                        y = get(yi)))
    Model <- lm(get(yi) ~ get(xi), data=datos)
    Graphp <- p + 
        geom_point() + 
        geom_smooth(method = lm, se = TRUE, color = "darkred") +
        theme_economist(base_size = 10, base_family = "sans", dkpanel = TRUE) +
        scale_colour_economist()
        
    Graphp2 <- Graphp + labs(x = xi, y = yi,
             title = paste('Gr醘ica regresi髇 lineal simple: ', 
                            xi, ' | ', yi, sep = ''),
             caption = paste('Intercepto =',signif(Model$coef[[1]],5), 
                                             '; Pendiente =',signif(Model$coef[[2]], 5),
                                             '; Adj R2 = ',signif(summary(Model)$adj.r.squared, 5),
                                             '; p =',signif(summary(Model)$coef[2,4], 5))) +
        theme(
            plot.title = element_text(size = 16, 
                                      face = "bold", 
                                      hjust = 0.5, 
                                      vjust=2.5),
            plot.caption = element_text(face = "italic", 
                                        size = 10, 
                                        hjust = 0.98, 
                                        vjust=-2.5),
            axis.ticks.length = unit(.15, "cm"),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(color = "black", 
                                        size = 10,
                                        face = "bold",
                                        vjust = -1),
            axis.title.y = element_text(color = "black",
                                        size = 10,
                                        face = "bold",
                                        vjust = 3)
        )
        #geom_rug() + geom_density_2d() +
    modelsumm <- (summary(Model))    
    print(modelsumm)
    print(modelsumm$coef)
    print(Graphp2)
}

RLsimple_f(geiserdat, 'waiting', 'eruptions')

