# ==============================================================================
# PREGUNTA D
# ==============================================================================

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

## 2.1 Arbol con criterio de ENTROPIA --------------------------------------

tree_entropy <- rpart(diagnosis ~ ., 
                      data = train,
                      parms = list(split = 'information'),
                      method = "class")

# Visualizar árbol
rpart.plot(tree_entropy, main = "Árbol - Entropía")

# Informacion del árbol
print(tree_entropy)
summary(tree_entropy)


## 2.2 Arbol con criterio de GINI ------------------------------------------

tree_gini <- rpart(diagnosis ~ ., 
                   data = train,
                   parms = list(split = 'gini'),
                   method = "class")

# Visualizar árbol
rpart.plot(tree_gini, main = "Árbol - Gini")


# Poda del árbol
tree_pruned <- prune(tree_gini, cp = optimal_cp)

# Comparar árboles
par(mfrow = c(1, 2))
rpart.plot(tree_gini, main = "Árbol Original")
rpart.plot(tree_pruned, main = "Árbol Podado")
par(mfrow = c(1, 1))


# 3. Evaluacion de modelos ---------------------------------------------------

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


# 4. Random Forest ---------------------------

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


# 5. Importancia de Variables ---------------------------------------------

# Obtener importancia
importance_data <- importance(rf_model)
print(importance_data)

# Gráfico de importancia
varImpPlot(rf_model, main = "Importancia de Variables - Random Forest")

# Gráfico de barras personalizado

barplot(rf_model$importance[,'MeanDecreaseAccuracy'],
        names.arg=names(rf_model$importance[,'MeanDecreaseAccuracy']))


# 6. Comparacion Final de Modelos -----------------------------------------

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


