#################################
############ Clase 39 ###########
#### Detección de Anomalías #####


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


#### Dataframe
df = readr::read_csv("ESalud19.csv", show_col_types = FALSE)
head(df)


#### Creación de data frame numérico:
df_s= df %>% 
   dplyr::mutate(SEXO_N = as.numeric(factor(SEXO, levels = c("MUJER", "HOMBRE"))),
                 FUMA_N = as.numeric(factor(FUMA, levels = c("SÍ","NO ESTÁ SEGURO","NO","NO RECUERDA" ))),
                 DIABETES_N = as.numeric(factor(DIABETES, levels = c("SÍ", "NO","NO RECUERDO")))) %>% 
   dplyr::select("SEXO","FUMA","DIABETES",
                 "SEXO_N","EDAD",
                 "PESO","ESTATURA",
                 "FUMA_N",
                 "DIABETES_N",
                 "GLUCOSA","SODIO_ORINA",
                 "COLESTEROL","PAS3","PAD3"
   ) %>% drop_na()

df_s["Id"] = 1:nrow(df_s)

df_1 = df_s %>%  dplyr::select("Id",
                               "SEXO","EDAD",
                               "PESO","ESTATURA",
                               "FUMA",
                               "DIABETES",
                               "GLUCOSA","SODIO_ORINA",
                               "COLESTEROL","PAS3","PAD3")

df_n = df_s %>%  dplyr::select(
   "SEXO_N","EDAD",
   "PESO","ESTATURA",
   "FUMA_N",
   "DIABETES_N",
   "GLUCOSA","SODIO_ORINA",
   "COLESTEROL","PAS3","PAD3")


head(df_1)

head(df_n)

###### Algoritmos de Distancia #######

#1. Distancia de Mahalanobis
md = mahalanobis(df_n,
                 colMeans(df_n),
                 cov(df_n))


bx_md = boxplot(md)


df_1 %>%
   mutate(dist_mahalanobis = md) %>%
   arrange(desc(dist_mahalanobis)) %>% head(10)


#2. Distancia KNN
dist_knn = get.knn(data = df_n, k=3)
media_grupo = rowMeans(dist_knn$nn.dist)

boxplot(media_grupo)

df_1 %>%
   mutate(dist_knn = media_grupo) %>%
   arrange(desc(dist_knn)) %>% head(10)



###### Algoritmos de Densidad #######

# 3.- Isolation Forest
# Ajustamos el modelo
set.seed(123)
isoforest_m <- isolation.forest(
   df_n,      # solo variables numéricas
   ntrees      = 1000,
   sample_size = "auto",
   ndim        = 1,   # proyección univariante (similar a sklearn por defecto)
   prob_pick_avg_gain = 0,  # para que se parezca más a sklearn
   prob_pick_pooled_gain = 0,
   nthreads    = 1)

# Scores de anomalía (valores más altos = más anómalos)
scores <- predict(isoforest_m, df_n, type = "score")

boxplot(scores)

df_1$dist_isoforest <- scores

# Top 10 casos más anómalos
head(df_1[order(-df_1$dist_isoforest), ], 10)

# 4.- Método LOF
# Ajustar el modelo LOF
# k = número de vecinos (equivale a n_neighbors en sklearn)
lof_scores <- lof(df_n, minPts = 20)

boxplot(lof_scores)

df_1$dist_lof <- lof_scores

# Valores más anómalos ---
head(df_1[order(-df_1$dist_lof), ], 10)


#5.- Método DBSCAN
# Ajustar DBSCAN
# eps = radio de vecindad
# minPts = mínimo de puntos para formar un cluster
db <- dbscan(df_n, eps = 70, minPts = 30)

# Veamos los clusters asignados
table(db$cluster)   # cuenta cuántos puntos en cada cluster
# Nota: los -1 de Python aquí aparecen como 0 (ruido/outliers)

df_1$dbscan_clust <- db$cluster

# Revisar
head(df_1)


#¿Cómo influye los datos atípicos en el pronóstico de un modelo de clasificación?

df3 = df_1 %>%
   dplyr::mutate(DIABETES_N = ifelse(DIABETES=="SÍ",1,0)) %>%
   dplyr::select("PESO","ESTATURA","GLUCOSA","SODIO_ORINA","PAS3","PAD3","EDAD","COLESTEROL","DIABETES_N")

modelo = glm(DIABETES_N~., data=df3)

#Caso I. Sin Outliers
probabilidades = predict(modelo, newdata = df3, type = "response")
predicciones = ifelse(probabilidades > 0.5, 1, 0)

confusionMatrix(data = factor(predicciones),
                reference = factor(df3$DIABETES_N), positive = "1")

#Caso II. Con Outlier
df_aux = df_1 %>%
   dplyr::select("PESO","ESTATURA","GLUCOSA","SODIO_ORINA","PAS3","PAD3","EDAD","COLESTEROL")
md = mahalanobis(df_aux, colMeans(df_aux),cov(df_aux))

df4 = df3 %>%
   dplyr::mutate(dummy_outlier = ifelse(md>30,1,0))

modelo2 = glm(DIABETES_N~., data=df4)

probabilidades2 = predict(modelo2, newdata = df4, type = "response")
predicciones2 = ifelse(probabilidades2 > 0.5, 1, 0)

confusionMatrix(data = factor(predicciones2),
                reference = factor(df4$DIABETES_N), positive = "1")
