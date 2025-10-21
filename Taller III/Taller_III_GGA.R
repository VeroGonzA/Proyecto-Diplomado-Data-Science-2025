##### PREGUNTA B #####

#Librerias
library(readr)
library(tidyverse)
library(plotly)
library(reticulate)
library(FNN)
install.packages("isotree")
library(isotree)
install.packages("dbscan")
library(dbscan)
install.packages("caret")
library(caret)

#BBDD
bbdd <- read.csv(file.choose(), stringsAsFactors = FALSE)
View(bbdd)

#Guardar identificadores y diagnostico
id <- bbdd$id
diagnostico <- bbdd$diagnosis

# Seleccionar solo variables numéricas
bbdd_n <- bbdd[ , !(names(bbdd) %in% c("id", "diagnosis")) ]

# 1. Distancia Mahalanobis
cov_mat <- cov(bbdd_n)
md <- mahalanobis(bbdd_n, colMeans(bbdd_n), cov_mat)
summary(md)

boxplot(md, main ="Distancia de Mahalanobis", ylab= "Distancia")


# 2. Isolation Forest
#Ajuste del Modelo
set.seed(14)
isoforest_m <- isolation.forest(
  bbdd_n,
  ntrees = 1000,
  sample_size = "auto",
  ndim = 1,
  prob_pick_avg_gain = 0,
  prob_pick_pooled_gain = 0,
  nthreads = 1)

scores_isof <- predict(isoforest_m, bbdd_n, type = "score")

boxplot(scores_isof, main = "Isolation Forest", ylab="Distancia")

# 3. Método LOF
lof_scores <- lof(bbdd_n, minPts = 20)
boxplot(lof_scores)

# Incorporar los puntajes a la base
bbdd_n$md <- md
bbdd_n$isoforest <- scores_isof
bbdd_n$lof <- lof_scores

# Incorporar los IDs y Diagnostico desde la base original
bbdd_n$id <- bbdd$id
bbdd_n$diagnosis <- bbdd$diagnosis

#Identificar Top 20 para cada método
top_md <- bbdd_n %>% arrange(desc(md)) %>% slice(1:20)
top_isof <- bbdd_n %>% arrange(desc(isoforest)) %>% slice(1:20)
top_lof <- bbdd_n %>% arrange(desc(lof)) %>% slice(1:20)

## El ejercicio pide al menos 5 IDs, con el top 10 solo tenia
## 3 IDs, pero aumentando a un Top 20 salen 6 IDs

# Extraer los IDs de cada top
ids_md <- top_md$id
ids_isof <- top_isof$id
ids_lof <- top_lof$id

ids_comunes <- Reduce(intersect, list(ids_md, ids_isof, ids_lof))
print(ids_comunes)

# Identificar datos morfologicos de los IDs
#PENDIENTE EL CODIGO

##### PREGUNTA C #####
library(tidyverse)
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

bbdd <- read.csv(file.choose(), stringsAsFactors = FALSE)

#Seleccionar variables que terminan en "_mean"
bbdd_c <- bbdd |>
  dplyr::select(ends_with("_mean"))

PerformanceAnalytics::chart.Correlation(bbdd_c, hist=TRUE, pch ="+")

# PCA
PCA <- prcomp(bbdd_c, scale. = TRUE)

# Visualización con factorextra
install.packages("factoextra")
library(factoextra)
fviz_pca_biplot(
  PCA,
  geom.ind = "point",
  col.ind = bbdd$diagnosis,
  palette = c("#00AFBB", "#E7B800"),
  addEllipses = TRUE,
  label = "var",
  repel = TRUE,
  legend.title = "Diagnóstico"
)


##### PREGUNTA D ####
install.packages("DAAG")
library(DAAG)
install.packages("splitTools")
library(splitTools)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("randomForest")
library(randomForest)
library(pROC)

# Carga de BBDD
bbdd <- read.csv(file.choose(), stringsAsFactors = TRUE)

# Etiqueta sin espacios
names(bbdd) <- gsub(" ", "_", names(bbdd))

#Semilla y entrenamiento (70%) y prueba (30%)
set.seed(2025)
bbdd_part <- partition(1:nrow(bbdd), p = c(0.7, 0.3))
bbdd_train <- bbdd[bbdd_part$`1`, ]
bbdd_test <- bbdd[bbdd_part$`2`, ]

# 1. ARBOL DE DECISION GINI
# Árbol con entropía
tree_entropy <- rpart(diagnosis ~ ., data = bbdd_train,
                      parms = list(split = 'information'),
                      method = "class")
rpart.plot(tree_entropy, main = "Árbol - Entropía")

# Árbol con Gini
tree_gini <- rpart(diagnosis ~ ., data = bbdd_train,
                   parms = list(split = 'gini'),
                   method = "class")
rpart.plot(tree_gini, main = "Árbol - Gini")

# Árbol con poda (Gini)
tree_gini_prune <- rpart(diagnosis ~ ., data = bbdd_train,
                         parms = list(split = 'gini'),
                         method = "class",
                         control = rpart.control(cp = 0.032))
rpart.plot(tree_gini_prune, main = "Árbol podado - Gini")

# Establecer la ventana gráfica en 1 fila y 2 columnas
par(mfrow = c(1, 2))

# Árbol con entropía
rpart.plot(tree_entropy, main = "Árbol - Entropía")

# Árbol con Gini
rpart.plot(tree_gini, main = "Árbol - Gini")

# Restaurar configuración gráfica por defecto
par(mfrow = c(1, 1))



# 2. RANDOM FOREST
set.seed(2025)
rf_model <- randomForest(diagnosis ~ ., data = bbdd_train,
                         ntree = 200, importance = TRUE)

# Importancia de variables
barplot(rf_model$importance[,"MeanDecreaseAccuracy"],
        las = 2, cex.names = 0.7,
        main = "Importancia de variables - Random Forest")

# 3. Curva ROC y comparación AUC
# Predicciones probabilísticas
probs_tree <- predict(tree_gini_prune, bbdd_test, type = "prob")[, "M"]
probs_rf   <- predict(rf_model, bbdd_test, type = "prob")[, "M"]

# Variable real
real <- ifelse(bbdd_test$diagnosis == "M", 1, 0)

# Curvas ROC
roc_tree <- roc(real, probs_tree)
roc_rf   <- roc(real, probs_rf)

# Graficar curvas ROC
plot(roc_tree, col = "blue", main = "Curvas ROC - Árbol vs Random Forest")
plot(roc_rf, col = "red", add = TRUE)
legend("bottomright", legend = c("Árbol de decisión", "Random Forest"),
       col = c("blue", "red"), lwd = 2)

# AUC
auc_tree <- auc(roc_tree)
auc_rf <- auc(roc_rf)

cat("AUC Árbol de decisión:", round(auc_tree, 3), "\n")
cat("AUC Random Forest:", round(auc_rf, 3), "\n")

