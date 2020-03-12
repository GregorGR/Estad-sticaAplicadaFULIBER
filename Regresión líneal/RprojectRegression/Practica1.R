library(ggplot2)
data(cars)
?cars
head(cars)
View(cars)
## Diagrama de dispersión
ggplot(cars, aes(x=speed, y=dist)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

## Mi primer modelo

Modelo1 = lm(dist~speed, data=cars)
summary(Modelo1)

### Ecuación de regresión estimada
## dist_estimada=-17.5791+3.9324*speed


