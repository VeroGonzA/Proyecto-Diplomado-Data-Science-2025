####PREGUNTA 1####
#¿Qué características de la base de datos, y 
#del problema en general, permite discriminar
#si se aplican técnicas de aprendizaje supervisado
#o aprendizaje no supervisado? ¿Se pueden aplicar
#ambas técnicas de aprendizaje? Fundamente.

##Respuesta: dado que la base de datos contiene la variable a predecir y que el
##problema está relacionado con encontrar patrones relevantes al momento de 
## de diagnosticar/tratar (decision medica) el cancer de mama, este ejercicio
## cumple con los criterios para poder aplicar tecnicas de análisis supervisado.
##aun así, es posible utilizar analisis no supervisados, en la medida que en el modelo 
##se omita la variable "Y", en este caso "diagnosis"


####PREGUNTA 2####
#Aplicando tres métodos de detección de valores atípicos:
#Distancia de Mahalanobis, Isolation Forest y
#Local Outlier Factor (LOF), 
#verifique si existen observaciones (IDs) 
#que hayan sido clasificadas como valores atípicos
#simultáneamente por los tres métodos. 
#En caso afirmativo, identifique al menos cinco de 
#esos IDs coincidentes. ¿Podrían considerarse estos 
#valores atípicos confiables dentro del conjunto de datos? 
#Justifique considerando el criterio de coincidencia entre métodos
#y la naturaleza del dataset (por ejemplo, posibles mediciones 
#extremas o patrones morfológicos inusuales en los tumores).

library(readr)
bbdd <- read_csv("Taller III/breast-cancer.csv")
View(bbdd)

#install.packages('tidyverse')
#install.packages('plotly')
#install.packages('reticulate')
#install.packages("googledrive")
#install.packages("FNN")
#install.packages("isotree")
#install.packages("dbscan")
#install.packages("caret")


library(tidyverse)
library(readxl)
library(plotly)
library(reticulate)
library(googledrive)
library(FNN)
library(isotree)
library(dbscan)
library(caret)
str(bbdd)
bbdd_n<-bbdd |> select(-id)
names(bbdd_n)

bbdd_n <- bbdd_n |> 
  mutate(across(-diagnosis, as.numeric),
         diagnosis=as.numeric(factor(diagnosis, levels=c("B","M"))))
 
#1. Distancia de Mahalanobis
md = mahalanobis(bbdd_n,
                 colMeans(bbdd_n),
                 cov(bbdd_n))


bx_md = boxplot(md)


bbdd<-bbdd %>%
  mutate(dist_mahalanobis = md)###outlier son los con mayor número



# 3.- Isolation Forest
# Ajustamos el modelo
set.seed(14)
isoforest_m <- isolation.forest(
  bbdd_n,      # solo variables numéricas
  ntrees      = 1000,
  sample_size = "auto",
  ndim        = 1,   # proyección univariante (similar a sklearn por defecto)
  prob_pick_avg_gain = 0,  # para que se parezca más a sklearn
  prob_pick_pooled_gain = 0,
  nthreads    = 1)

# Scores de anomalía (valores más altos = más anómalos)
scores <- predict(isoforest_m, bbdd_n, type = "score")

boxplot(scores)

bbdd$dist_isoforest <- scores

# Top 10 casos más anómalos
head(bbdd[order(-bbdd$dist_isoforest), ], 10)###outlier son los con valor más bajo


# 4.- Método LOF
# Ajustar el modelo LOF
# k = número de vecinos (equivale a n_neighbors en sklearn)
lof_scores <- lof(bbdd_n, minPts = 20)

boxplot(lof_scores)

bbdd$dist_lof <- lof_scores

# Valores más anómalos ---
head(bbdd_n[order(-bbdd_n$dist_lof), ], 10)


bbdd_b<-bbdd |> 
  mutate(ranking_1=rank(-dist_mahalanobis),
         ranking_2=rank(-dist_isoforest),
         ranking_3=rank(-dist_lof))
bbdd_b<-bbdd_b |> 
  mutate(ranking_outlier=ranking_1+ranking_2+ranking_3) |> 
  arrange(ranking_outlier)

ranking_1<-bbdd_b |> 
  filter(ranking_1<=10)
ranking_2<-bbdd_b |> 
  filter(ranking_2<=10)
ranking_3<-bbdd_b |> 
  filter(ranking_3<=10)


outlier<-ranking_1 |> 
  full_join(ranking_2) |> 
  full_join(ranking_3)

###utilizando un criterio de los 10 mas outlier de cada ranking, sólo hay 3 ids que cumplen conla condicion en
###todos los rankings
###me falta elaborar sin ¿Podrían considerarse estos valores atípicos confiables dentro del conjunto de datos?
#Justifique considerando el criterio de coincidencia entre métodos y la naturaleza del dataset (por ejemplo, 
#posibles mediciones extremas o patrones morfológicos inusuales en los tumores).


####PREGUNTA 3####
#Realice una reducción de dimensión a solo 2 
#variables de las 10 variables de contexto 
#usando las variables medias (_mean), 
#grafique en el plano y use un color para
#distinguir los dos valores de la variable
#diagnóstico. ¿Cree que se logra encontrar 
#alguna separación entre ambos valores?
library(tidyverse)
library("PerformanceAnalytics")
bbdd_c<-bbdd |> 
  select(c(ends_with("_mean")))
head(bbdd_c)
str(bbdd_c)

PerformanceAnalytics::chart.Correlation(bbdd_c, hist = T, pch ="+")

## Cálculo de las componentes principales:
PCA <- prcomp(bbdd_c, scale = TRUE) #scale=TRUE estandariza las vari
library(factoextra) #Librería factoextra es muy útil para gráficos
fviz_pca_biplot(
  PCA,
  geom.ind = "point",             # mostrar puntos (individuos)
  col.ind = bbdd$diagnosis,    # color por grupo
  palette = c("#00AFBB", "#E7B800"), # paleta personalizada
  addEllipses = TRUE,             # elipses por grupo
  label = "var",                  # mostrar nombres de variables
  repel = TRUE,                   # evitar superposición de etiquetas
  legend.title = "Diagnóstico"
)

###segun esto, hay una especie de separacion aunque con una superposicion.
###parece ser que la Dim 1 (54% de la varianza) discrimina mejor entre grupos
###considerando que el grupo benigno está mas a la derecha y el grupo maligno más a la izquierda
###la Dim 2 es menos discriminadora, pues los puntos tanto benignos como malignos se distribuyen uniformemente en los ejes de arriba y abajo


####PREGUNTA 4 ####

#Suponiendo que el conjunto de datos fue dividido en entrenamiento y validación 
#en proporciones 70% - 30% y utilizando la semilla 2025. Realice un árbol de decisión 
#y un random forest para predecir el tipo de cáncer (diagnosis),
#obtenga las probabilidades en el conjunto de prueba, 
#trace las curvas ROC y compare los AUC entre ambos modelos,
#comentando cuál desempeña mejor y por qué.

