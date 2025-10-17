# Librerías
library(DAAG)
library(splitTools)
set.seed(2025)

# Partición de datos (70% entrenamiento, 30% prueba)
ind_train <- partition(1:nrow(spam7), p=c(0.7,0.3))
train <- spam7[ind_train$`1`,]
test  <- spam7[ind_train$`2`,]

# Árboles de decisión
library(rpart); library(rpart.plot)

# Árbol con entropía
tree_entropy <- rpart(yesno ~ ., data = train,
                      parms = list(split = 'information'),
                      method = "class")
rpart.plot(tree_entropy)

# Árbol con índice de Gini
tree_gini <- rpart(yesno ~ ., data = train,
                   parms = list(split = 'gini'),
                   method = "class")
rpart.plot(tree_gini)

# Árbol con poda (Gini)
tree_gini_prune <- rpart(yesno ~ ., data = train,
                         parms = list(split = 'gini'),
                         method = "class",
                         control = rpart.control(cp = 0.032))
rpart.plot(tree_gini_prune)

# Random Forest
library(randomForest)
set.seed(2025)
RanForClas <- randomForest(yesno ~ ., data = train,
                           ntree = 200, importance = TRUE)
barplot(RanForClas$importance[,'MeanDecreaseAccuracy'],
        names.arg = names(RanForClas$importance[,'MeanDecreaseAccuracy']))

# --------------------------
# MATRICES DE CONFUSIÓN + EXACTITUD
# --------------------------

# Predicciones en test
pred_entropy     <- predict(tree_entropy,     newdata = test, type = "class")
pred_gini        <- predict(tree_gini,        newdata = test, type = "class")
pred_gini_prune  <- predict(tree_gini_prune,  newdata = test, type = "class")
pred_rf          <- predict(RanForClas,       newdata = test, type = "class")

# Función para calcular exactitud
calc_acc <- function(pred, real) {
  mean(pred == real)
}

# Imprimir resultados
cat("\nMatriz de confusión - Árbol Entropía:\n")
print(table(Predicho = pred_entropy, Real = test$yesno))
cat("Exactitud:", round(calc_acc(pred_entropy, test$yesno), 4), "\n")

cat("\nMatriz de confusión - Árbol Gini:\n")
print(table(Predicho = pred_gini, Real = test$yesno))
cat("Exactitud:", round(calc_acc(pred_gini, test$yesno), 4), "\n")

cat("\nMatriz de confusión - Árbol Gini Podado:\n")
print(table(Predicho = pred_gini_prune, Real = test$yesno))
cat("Exactitud:", round(calc_acc(pred_gini_prune, test$yesno), 4), "\n")

cat("\nMatriz de confusión - Random Forest:\n")
print(table(Predicho = pred_rf, Real = test$yesno))
cat("Exactitud:", round(calc_acc(pred_rf, test$yesno), 4), "\n")