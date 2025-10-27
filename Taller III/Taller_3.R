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
names(bbdd) <- gsub(" ", "_", names(bbdd))


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

bbdd_b<-bbdd |> select(-id)
names(bbdd_b)

bbdd_b <- bbdd_b |> 
  mutate(across(-diagnosis, as.numeric),
         diagnosis=as.numeric(factor(diagnosis, levels=c("B","M"))))
 
#estandarizar variables para no sesgar el análisis

bbdd_st <- bbdd_b |>
  mutate(
    across(
      .cols = where(is.numeric) & !diagnosis,
      .fns  = ~ as.numeric(scale(.x))
    )
  )


summary(bbdd_st)

#1. Distancia de Mahalanobis
md = mahalanobis(bbdd_st,
                 colMeans(bbdd_st),
                 cov(bbdd_st))


bx_md = boxplot(md)
bbdd_b$dist_mahalanobis <- md


#bbdd_b<-bbdd_st %>%
#  mutate(dist_mahalanobis = md)###outlier son los con mayor número



# 3.- Isolation Forest
# Ajustamos el modelo
set.seed(14)
isoforest_m <- isolation.forest(
  bbdd_st,      # solo variables numéricas
  ntrees      = 1000,
  sample_size = "auto",
  ndim        = 1,   # proyección univariante (similar a sklearn por defecto)
  prob_pick_avg_gain = 0,  # para que se parezca más a sklearn
  prob_pick_pooled_gain = 0,
  nthreads    = 1)

# Scores de anomalía (valores más altos = más anómalos)
scores <- predict(isoforest_m, bbdd_st, type = "score")

boxplot(scores)

bbdd_b$dist_isoforest <- scores

# Top 10 casos más anómalos
head(bbdd[order(-bbdd$dist_isoforest), ], 10)###outlier son los con valor más bajo


# 4.- Método LOF
# Ajustar el modelo LOF
# k = número de vecinos (equivale a n_neighbors en sklearn)

###estandarizar variables
###criterio intercuartil
### criterio 97,5
lof_scores <- lof(bbdd_st, minPts = 20)

boxplot(lof_scores)

bbdd_b$dist_lof <- lof_scores

# Valores más anómalos ---
#head(bbdd_n[order(-bbdd_n$dist_lof), ], 10)


bbdd_b$ID<-bbdd$id

bbdd_b<-bbdd_b |> 
  mutate(ranking_1=rank(-dist_mahalanobis),
         ranking_2=rank(-dist_isoforest),
         ranking_3=rank(-dist_lof))
bbdd_b<-bbdd_b |> 
  mutate(ranking_outlier=ranking_1+ranking_2+ranking_3) |> 
  arrange(ranking_outlier)

ranking_1<-bbdd_b |> 
  filter(ranking_1<=20)
ranking_2<-bbdd_b |> 
  filter(ranking_2<=20)
ranking_3<-bbdd_b |> 
  filter(ranking_3<=20)


outlier<-ranking_1 |> 
  full_join(ranking_2) |> 
  full_join(ranking_3)

###segun intercuartil

umbral_1 <- quantile(bbdd_b$dist_isoforest, 0.95)

bbdd_b <- bbdd_b|>
  mutate(outlier_1 = dist_isoforest > umbral_1)

umbral_2 <- quantile(bbdd_b$dist_mahalanobis, 0.95)

bbdd_b <- bbdd_b|>
  mutate(outlier_2 = dist_mahalanobis > umbral_2)

umbral_3 <- quantile(bbdd_b$dist_lof, 0.95)

bbdd_b <- bbdd_b|>
  mutate(outlier_3 = dist_lof > umbral_3)

outlier_int<-bbdd_b |> 
  mutate(out_int=ifelse(outlier_1==TRUE & outlier_2==TRUE & outlier_3==TRUE, 1, 0)) |> 
  filter(out_int==1)

##selecciono los 5 primeros del criterio 20 en cada uno, que ademas están en el umbral .95 de los tres modelos
out_select<-outlier_int |> 
  head(5)

bbdd_modelo<-bbdd |> 
  mutate(diagnosis = as.numeric(factor(diagnosis, levels = c("B", "M"))) - 1) |> 
  select(-id)

##modelo con todos los IDs  
modelo_1 = glm(diagnosis~., data=bbdd_modelo)


probabilidades_1 = predict(modelo_1, newdata = bbdd_modelo, type = "response")
predicciones_1 = ifelse(probabilidades_1 > 0.5, 1, 0)

confusionMatrix(data = factor(predicciones_1),
                reference = factor(bbdd_modelo$diagnosis), positive = "1")

##modelo sin los 5 ids


bbdd_sinout<-bbdd |> 
  anti_join(out_select |> select(ID), by=c("id"="ID")) |> 
  mutate(diagnosis = as.numeric(factor(diagnosis, levels = c("B", "M"))) - 1) |> 
  select(-id)

modelo_2=glm(diagnosis~., data=bbdd_sinout)


probabilidades_2 = predict(modelo_2, newdata = bbdd_sinout, type = "response")
predicciones_2 = ifelse(probabilidades_2 > 0.5, 1, 0)

confusionMatrix(data = factor(predicciones_2),
                reference = factor(bbdd_sinout$diagnosis), positive = "1")


probabilidades_3 = predict(modelo_2, newdata = bbdd_modelo, type = "response")
predicciones_3 = ifelse(probabilidades_3 > 0.5, 1, 0)

confusionMatrix(data = factor(predicciones_3),
                reference = factor(bbdd_modelo$diagnosis), positive = "1")




###considerando que un modelo predictivo sin los outlier predice los mismo

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

library(DAAG)
library(splitTools)
set.seed(2025)
names(bbdd) <- gsub(" ", "_", names(bbdd))
bbdd_c<-bbdd |> 
  mutate(diagnosis = factor(diagnosis, levels = c("B", "M")))
  

bbdd_part <- partition(1:nrow(bbdd_c), p=c(0.7,0.3))
bbdd_train <- bbdd_c[bbdd_part$`1`,]
bbdd_test  <- bbdd_c[bbdd_part$`2`,]
library(rpart); library(rpart.plot)



# Árbol con entropía
tree_entropy <- rpart(diagnosis ~ ., data = bbdd_train,
                      parms = list(split = 'information'),
                      method = "class")
rpart.plot(tree_entropy)

tree_gini <- rpart(diagnosis ~ ., data = bbdd_train,
                   parms = list(split = 'gini'),
                   method = "class")
rpart.plot(tree_gini)

# Árbol con poda (Gini)
tree_gini_prune <- rpart(diagnosis ~ ., data = bbdd_train,
                         parms = list(split = 'gini'),
                         method = "class",
                         control = rpart.control(cp = 0.032))
rpart.plot(tree_gini_prune)

# Random Forest
library(randomForest)
set.seed(2025)
RanForClas <- randomForest(diagnosis ~ ., data = bbdd_train,###esta linea me da error
                           ntree = 200, importance = TRUE)
barplot(RanForClas$importance[,'MeanDecreaseAccuracy'],
        names.arg = names(RanForClas$importance[,'MeanDecreaseAccuracy']))


####

prob_entropy <- predict(tree_entropy, bbdd_test, type = "prob")[,2]
prob_gini<- predict(tree_gini, bbdd_test, type = "prob")[,2]
prob_gini_prune   <- predict(tree_gini_prune, bbdd_test, type = "prob")[,2]
prob_rf     <- predict(RanForClas, bbdd_test, type = "prob")[,2]

library(pROC)

roc_entropy <- roc(bbdd_test$diagnosis, prob_entropy)
roc_gini<- roc(bbdd_test$diagnosis, prob_gini)
roc_gini_prune   <- roc(bbdd_test$diagnosis, prob_gini_prune)
roc_rf<- roc(bbdd_test$diagnosis, prob_rf)



plot(roc_entropy, col = "purple",  lwd = 2, main = "Curvas ROC comparadas")
plot(roc_gini, col = "violet", lwd = 2, add = TRUE)
plot(roc_gini_prune,     col = "magenta",   lwd = 2, add = TRUE)
plot(roc_rf,     col = "pink",   lwd = 2, add = TRUE)

legend("bottomright",
       legend = c(paste("Árbol Entropía (AUC =", round(auc(roc_entropy), 3), ")"),
                  paste("Árbol Gini (AUC =", round(auc(roc_gini), 3), ")"),
                  paste("Árbol Gini Podado (AUC =", round(auc(roc_gini_prune),3),")"),
                  paste("Random Forest (AUC =", round(auc(roc_rf), 3),")")),
       col = c("purple", "violet", "magenta","pink"),
       lwd = 2)



