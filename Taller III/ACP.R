######### Clase 36-37
######### Reducción de dimensionalidad: 

######################################
# Análisis de Componentes Principales
########################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

## Carga de datos:
songs <- openxlsx::read.xlsx("songs.xlsx")
library("PerformanceAnalytics")
#songs = readxl::read_excel("songs.xlsx")
songs<-bbdd_b
head(songs)
str(songs)

PerformanceAnalytics::chart.Correlation(songs, hist = T, pch ="+")

## Cálculo de las componentes principales:
PCA <- prcomp(songs, scale = TRUE) #scale=TRUE estandariza las vari

library(factoextra) #Librería factoextra es muy útil para gráficos
get_eigenvalue(PCA) #Ya con 4 componentes (de las 7) explicamos el 75%

##Scree Plot - Variabilidad por componente    
fviz_eig(PCA,main="Varianza explicada por componente",
         xlab="Componente",ylab="Porcentaje de varianza explicada (PVE) ",
         linecolor = "navyblue",
         barfill = "dodgerblue3",ylim=c(0,100),addlabels=TRUE)

## Contribución de las variables en las primeras componentes 
fviz_pca_var(PCA, col.var = "deepskyblue2")

## Contribución de las variables en todas las componentes 
Comp <- get_pca_var(PCA)
library(corrplot)
corrplot(Comp$contrib, is.corr = FALSE, tl.col = "darkcyan")

## Variables proyectadas 
head(scale(songs)%*%PCA$rotation[,1:2]) #Vectores proyectados

corrplot(cor(scale(songs)%*%PCA$rotation[,1:2])) #Son no correlacionadas


######################################
# Análisis Factorial
########################################
food<-bbdd_b
## Carga de datos:
library(readr)
food_texture <- read_csv("food-texture.csv")
food <- food_texture[, -1] #Quitamos la primera columna o id
food <- scale(food) #Se estandarizan las variables
head(food)
str(food)
library(PerformanceAnalytics)
PerformanceAnalytics::chart.Correlation(food, hist = T, pch ="+")


## Cálculo de los factores comunes sin rotar

food.fa.none <- factanal(food, factors = 2, rotation = "none")
food.fa.none

## Comunalidad y Unicidad 
apply(food.fa.none$loadings^2,1,sum) #Comunalidad
1 - apply(food.fa.none$loadings^2,1,sum) # Unicidad
food.fa.none$uniquenesses

## Análisis factorial vectores rotados  
food.fa.varimax <- factanal(food, factors = 2, rotation = "varimax")
food.fa.varimax

## Comunalidad y Unicidad 
apply(food.fa.varimax$loadings^2,1,sum) #Comunalidad
1 - apply(food.fa.varimax$loadings^2,1,sum) # Unicidad
food.fa.varimax$uniquenesses


### escores
factanal(food, factors = 2, rotation = "varimax",scores = "Bartlett")$scores

##### gráfico de factores sin rotar y rotados

par(mfrow = c(1,2))
plot(food.fa.none$loadings[,1], food.fa.none$loadings[,2],
     xlab = "Factor 1", ylab = "Factor 2", 
     ylim = c(-1,1), xlim = c(-1,1),
     main = "Sin rotación")
text(food.fa.none$loadings[,1]-0.08, food.fa.none$loadings[,2]+0.08,
     colnames(food), col="blue")
abline(h = 0, v = 0)

plot(food.fa.varimax$loadings[,1], food.fa.varimax$loadings[,2],
     xlab = "Factor 1", ylab = "Factor 2", 
     ylim = c(-1,1), xlim = c(-1,1),
     main = "Rotación Varimax")



