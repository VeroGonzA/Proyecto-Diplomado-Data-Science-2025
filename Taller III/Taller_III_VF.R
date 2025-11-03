# PREGUNTA B -------------------------------------------------------------------

# 1. Preparacion de Datos ------------------------------------------------------

library(readr)
bbdd <- read_csv("Taller III/breast-cancer.csv")
View(bbdd)
names(bbdd) <- gsub(" ", "_", names(bbdd))

## 1.a Librerias Necesarias -----------------------------------------------------
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

## 1.b Estandarizar Variables  --------------------------------------------------

bbdd_st <- bbdd_b |>
  mutate(
    across(
      .cols = where(is.numeric) & !diagnosis,
      .fns  = ~ as.numeric(scale(.x))
    )
  )


summary(bbdd_st)

# 2. Distancia de Mahalanobis --------------------------------------------------
md = mahalanobis(bbdd_st,
                 colMeans(bbdd_st),
                 cov(bbdd_st))

bx_md = boxplot(md)
bbdd_b$dist_mahalanobis <- md

#bbdd_b<-bbdd_st %>%
#  mutate(dist_mahalanobis = md)###outlier son los con mayor número

# 3. Isolation Forest ----------------------------------------------------------
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

# 4. Método LOF ----------------------------------------------------------------

lof_scores <- lof(bbdd_st, minPts = 20)

boxplot(lof_scores)

bbdd_b$dist_lof <- lof_scores

# 5. Valores Anómalos ----------------------------------------------------------
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

# 6. Top 5 Anómalos ----------------------------------------------------------------

## selecciono los 5 primeros del criterio 20 en cada uno, que ademas están 
## en el umbral .95 de los tres modelos

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

# PREGUNTA C -------------------------------------------------------------------

# 1. Preparacion de Datos ------------------------------------------------------
library(tidyverse)
library("PerformanceAnalytics")
bbdd_c<-bbdd |> 
  select(c(ends_with("_mean")))
head(bbdd_c)
str(bbdd_c)

PerformanceAnalytics::chart.Correlation(bbdd_c, hist = T, pch ="+")

# 2. Cálculo de los componentes principales  -----------------------------------

PCA <- prcomp(bbdd_c, scale = TRUE) #scale=TRUE estandariza las vari
library(factoextra) #Librería factoextra es muy útil para gráficos
fviz_pca_biplot(
  PCA,
  geom.ind = "point",                # mostrar puntos (individuos)
  col.ind = bbdd$diagnosis,          # color por grupo
  palette = c("#00AFBB", "#E7B800"), # paleta personalizada
  addEllipses = TRUE,                # elipses por grupo
  label = "var",                     # mostrar nombres de variables
  repel = TRUE,                      # evitar superposición de etiquetas
  legend.title = "Diagnóstico"
)

# PREGUNTA D ----------------------------------------------------
# Librerias Necesarias ----------------------------------------------------

library(rpart)          # Árboles de decisión
library(rpart.plot)     # Visualización de árboles
library(splitTools)     # Para dividir datos
library(randomForest)   # Random Forest
library(caret)          # Métricas de evaluación
library(tidyverse)      # Manipulación de la bbdd

# 1. Preparacion de Datos -------------------------------------------------
bbdd <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Cargar datos
head(bbdd)
str(bbdd)

# Verificar distribución de la variable objetivo
table(bbdd$diagnosis)
prop.table(table(bbdd$diagnosis))

# División train/test
names(bbdd) <- gsub(" ", "_", names(bbdd))

set.seed(2025)

bbdd_c<-bbdd |> 
  mutate(diagnosis = factor(diagnosis, levels = c("B", "M")))


bbdd_part <- partition(1:nrow(bbdd_c), p=c(0.7,0.3))
train <- bbdd_c[bbdd_part$`1`,]
test  <- bbdd_c[bbdd_part$`2`,]

nrow(train)
nrow(test)

# 2. Metodos de ramificacion ----------------------------------------------

## 2.a Arbol con criterio de ENTROPIA --------------------------------------

tree_entropy <- rpart(diagnosis ~ ., 
                      data = train,
                      parms = list(split = 'information'),
                      method = "class")

# Visualizar árbol
rpart.plot(tree_entropy, main = "Árbol - Entropía")

# Informacion del árbol
print(tree_entropy)
summary(tree_entropy)

## 2.b Arbol con criterio de GINI ------------------------------------------

tree_gini <- rpart(diagnosis ~ ., 
                   data = train,
                   parms = list(split = 'gini'),
                   method = "class")

# Visualizar árbol
rpart.plot(tree_gini, main = "Árbol - Gini")


# 3. Analisis de CP -------------------------------------------------------

# Mostrar tabla de complejidad
printcp(tree_gini)

# Gráfico de complejidad
plotcp(tree_gini)

# Obtener CP óptimo (regla 1-SE)
cp_table <- tree_gini$cptable
min_error_index <- which.min(cp_table[, "xerror"])
min_error <- cp_table[min_error_index, "xerror"]
min_std <- cp_table[min_error_index, "xstd"]

# CP óptimo según regla 1-SE
optimal_cp_index <- which(cp_table[, "xerror"] <= min_error + min_std)[1]
optimal_cp <- cp_table[optimal_cp_index, "CP"]

cat("CP óptimo (regla 1-SE):", optimal_cp, "\n")

# Poda del árbol
tree_pruned <- prune(tree_gini, cp = optimal_cp)

# Comparar árboles
par(mfrow = c(1, 2))
rpart.plot(tree_gini, main = "Árbol Original")
rpart.plot(tree_pruned, main = "Árbol Podado")
par(mfrow = c(1, 1))

# 4. Evaluacion de modelos ---------------------------------------------------

# Predicciones
pred_entropy <- predict(tree_entropy, test, type = "class")
pred_gini <- predict(tree_gini, test, type = "class")
pred_pruned <- predict(tree_pruned, test, type = "class")

# Métricas de evaluación
accuracy_entropy <- mean(pred_entropy == test$diagnosis)
accuracy_gini <- mean(pred_gini == test$diagnosis)
accuracy_pruned <- mean(pred_pruned == test$diagnosis)

# Matrices de confusión

#Arbol bajo criterio de Entropia
cm_entropy <- confusionMatrix(pred_entropy, test$diagnosis)
print(cm_entropy)

#Arbol bajo criterio de Gini
cm_gini <- confusionMatrix(pred_gini, test$diagnosis)
print(cm_gini)

#Arbol Podado
cm_pruned <- confusionMatrix(pred_pruned, test$diagnosis)
print(cm_pruned)

# Resumen de accuracy
cat("\nRESUMEN DE ACCURACY:\n")
cat("Entropía:", round(accuracy_entropy, 4), "\n")
cat("Gini:", round(accuracy_gini, 4), "\n")
cat("Podado:", round(accuracy_pruned, 4), "\n")


# 5. Random Forest ---------------------------

# Entrenar Random Forest
set.seed(2025)
rf_model <- randomForest(diagnosis ~ ., 
                         data = train,
                         ntree = 100,
                         importance = TRUE)

print(rf_model)

# Predicciones Random Forest
pred_rf <- predict(rf_model, test)
accuracy_rf <- mean(pred_rf == test$diagnosis)
accuracy_rf

# Matriz de confusión Random Forest
cm_rf <- confusionMatrix(pred_rf, test$diagnosis)
print(cm_rf)


# 6. Importancia de Variables ---------------------------------------------

# Obtener importancia
importance_data <- importance(rf_model)
print(importance_data)

# Gráfico de importancia
varImpPlot(rf_model, main = "Importancia de Variables - Random Forest")

# Gráfico de barras personalizado
importancias <- importance_data[ , "MeanDecreaseAccuracy"]
importancias_ord <- sort(importancias, decreasing = TRUE)

barplot(importancias_ord,
        names.arg = names(importancias_ord),
        las = 3,
        cex.names = 0.75,  
        main = "Importancia de Variables - MeanDecreaseAccuracy",
        ylab = "MeanDecreaseAccuracy",
        ylim = c(0, max(importancias_ord) * 1.12))

# 7. Comparacion Final de Modelos -----------------------------------------

# Crear tabla comparativa
results <- data.frame(
  Modelo = c("Árbol Entropía", "Árbol Gini", "Árbol Podado", "Random Forest"),
  Accuracy = c(accuracy_entropy, accuracy_gini, accuracy_pruned, accuracy_rf),
  stringsAsFactors = FALSE
)

print(results)

# 7. Curva ROC -----------------------------------------#

test$diagnosis <- factor(test$diagnosis, levels = c("B", "M"))

# Obtener probabilidades para clase "M"
prob_entropy <- predict(tree_entropy, test, type = "prob")[, "M"]
prob_gini <- predict(tree_gini, test, type = "prob")[, "M"]
prob_pruned <- predict(tree_pruned, test, type = "prob")[, "M"]
prob_rf <- predict(rf_model, test, type = "prob")[, "M"]

# Calcular curvas ROC

library(pROC)

roc_entropy <- roc(response = test$diagnosis, predictor = prob_entropy, levels = c("B", "M"), direction = "<")
roc_gini <- roc(response = test$diagnosis, predictor = prob_gini, levels = c("B", "M"), direction = "<")
roc_pruned <- roc(response = test$diagnosis, predictor = prob_pruned, levels = c("B", "M"), direction = "<")
roc_rf <- roc(response = test$diagnosis, predictor = prob_rf, levels = c("B", "M"), direction = "<")

# Graficar curvas ROC
plot(roc_entropy, col = "#3B0270", lwd = 2, main = "Curvas ROC comparadas")
plot(roc_gini, col = "#DF42D1", lwd = 2, add = TRUE)
plot(roc_pruned, col = "#EEA5F6", lwd = 2, add = TRUE)
plot(roc_rf, col = "#6F00FF", lwd = 2, add = TRUE)

# Leyenda con AUC
legend("bottomright",
       legend = c(paste("Árbol Entropía (AUC =", round(auc(roc_entropy), 3), ")"),
                  paste("Árbol Gini (AUC =", round(auc(roc_gini), 3), ")"),
                  paste("Árbol Podado (AUC =", round(auc(roc_pruned), 3), ")"),
                  paste("Random Forest (AUC =", round(auc(roc_rf), 3), ")")),
       col = c("#3B0270", "#DF42D1", "#EEA5F6", "#6F00FF"),
       lwd = 3)