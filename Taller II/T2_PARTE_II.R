# Librerías necesarias
library(readxl)      # para leer Excel
library(dplyr)       # manipulación
library(purrr)       # map()
library(broom)       # tidy resultados
library(pROC)        # AUC y ROC

dpm <- read_excel("Taller II/dpm.xlsx")
names(dpm)
names(dpm)<-c("Caso",              
              "FrecCardiaca",    
              "PresionSistol",      
              "Dosis",              
              "Edad",               
              "Sexo",               
              "baseEF",             
              "DolorPecho",         
              "SE",                 
              "Hiperten",           
              "Diabetes",           
              "Fuma",               
              "Pre_MI",             
              "Evento",
              "ECG")
table(dpm$Evento)/ nrow(dpm)
###hago la división de la base en entrenamiento y prueba 70/30
set.seed(14)
train <- dpm %>%
  dplyr::sample_frac(0.70)

table(train$Evento) / nrow(train)

test <- dplyr::anti_join(dpm, train, by = "Caso")

#newdata <- data.frame(x1 = test$x1)

table(test$Evento) / nrow(test)



####
predictores <- c("FrecCardiaca", "PresionSistol", "Dosis", "Edad", "Sexo",
                 "Hiperten", "Diabetes", "Fuma", "baseEF", "DolorPecho",
                 "SE", "Pre_MI", "ECG")



# 3. Generar todas las combinaciones posibles de 2 predictores
comb <- combn(predictores, 2, simplify = FALSE)

# 4. Evaluar cada modelo
### hace una funcion que evalua todos (78) modelos posibles con 2 combinaciones 
### a eso les calcula el AIC, BIC y AUC con datos de entrenamiento
##AIC: comparacion entre modelos con la misma variable dependiente, mientras más bajo el modelo es mejor en terminos de ajuste
#parsimonia (esto ultimo no se si aplica si consideramos que todos tiene 2 variables)
##BIC:medida más conservadora para comparar modelos, considerando que penaliza más por el número de parámetros (esto no aplicaría cuando estamos considerando 2 variables en cada modelo)
##AUC: evalua la capacidad predictiva del modelo para distinguir entre clases. 
#lo mínimo es que sea mayor a 0.5 pues eso equivale a lanzar una moneda. de 0.7 en adelante es aceptable
resultados <- map_df(comb, function(vars) {
  f <- as.formula(paste("Evento ~", paste(vars, collapse = " + ")))
  modelo <- glm(f, data = train, family = binomial(link = logit))
  
  # Predicciones
  prob <- predict(modelo, type = "response")
  roc_train <- roc(train$Evento, prob, quiet = TRUE)
  
  tibble(
    vars = paste(vars, collapse = " + "),
    AIC = AIC(modelo),
    BIC = BIC(modelo),
    AUC_train = as.numeric(auc(roc_train)),
    modelo = list(modelo)
  )
})
###mostrar en una tabla los resultados de los 78 modelos?


# 5. Seleccionar el mejor modelo (menor AIC y mayor AUC si empate)
###Según los criterios de AIC, BIC y AUC, se selecciona el mejor modelo entre las 78 combinaciones posibles:
##menor AIC y BIC y mayor AUC
##en este caso, los tres criterios dan 1 solo modelo
## baseEF - estimación de la fracción de Dopamina en el ventrículo izquierdo 
##SE = respuesta positiva durante el ecocardiograma de esfuerzo (1=si, 0=no). 

mejor <- resultados %>%
  arrange(AIC, desc(AUC_train)) %>%
  slice(1)

# 6. Extraer coeficientes, OR e IC95% del mejor modelo
modelo_final <- mejor$modelo[[1]]

tabla_coef <- tidy(modelo_final, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>%
  mutate(term = ifelse(term == "(Intercept)", "Intercepto", term)) %>%
  rename(OR = estimate, IC_Inf = conf.low, IC_Sup = conf.high)

##de esto se deprende que:
# baseEF: tiene una relacion negativa y significativa (0.008) con la probabilidad del evento, es decir,
#por cada 1% de aumento en baseEF disminuye en 3,3% la probabilidad del evento (100*(1-0.967))

# SE: tiene una relacion positiva y signidicativa (0.0001) con la probabilidad del evento,
# al ser dictómica se interpreta como: si un paciente tiene SE positivo (=1) tiene 4.8 veces mas de odds de presentar un evento cardiaco en los 12 meses siguientes,
#en comparacion a pacientes que tienen SE negativo (=0)

# 7. Armar tabla final con AUC
tabla_final <- tabla_coef %>%
  mutate(AUC_train = round(mejor$AUC_train, 3),###este es el AUC considerando 
         #los mismos datos que se usaron para entrenar,
         #por lo que no se si tiene mucho sentido interpretarlo
         Modelo = mejor$vars)

print(tabla_final)


############CREO QUE ESTOS PUNTOS SON IMPORTANTES PARA EVALUAR EL MODELO
########PERO NO PARA LA PREGUNTA EN CONCRETO
# 8. Ver el poder predictivo del modelo en los datos de prueba
prob_test <- predict(modelo_final, newdata = test, type = "response")
roc_test <- roc(test$Evento, prob_test, quiet = TRUE)

# 9. Graficar curvas ROC segun los datos que se usan en entrenamiento y prueba
par(mfrow = c(1,2)) # dos gráficos lado a lado

# ROC en entrenamiento
prob_train <- predict(modelo_final, newdata = train, type = "response")
roc_train <- roc(train$Evento, prob_train, quiet = TRUE)
plot(roc_train, col = "blue", lwd = 2,
     main = paste("ROC Entrenamiento\nModelo:", mejor$vars))
legend("bottomright", legend = paste("AUC =", round(auc(roc_train), 3)),
       col = "blue", lwd = 2)

# ROC en prueba
plot(roc_test, col = "red", lwd = 2,
     main = paste("ROC Prueba\nModelo:", mejor$vars))
legend("bottomright", legend = paste("AUC =", round(auc(roc_test), 3)),
       col = "red", lwd = 2)


#Obtenga el mejor modelo de regresión logística (con todas las variables que sean significativas (2 puntos).

##hago el modelo con todas las variables: a primera vista sólo Hiperten, baseEF y SE son significativas
modelo_completo <- glm(Evento ~ FrecCardiaca + PresionSistol + Dosis +
                         Edad + Sexo + Hiperten + Diabetes + Fuma +
                         baseEF + DolorPecho + SE + Pre_MI + ECG,
                       data = train, family = binomial(link = logit))
summary(modelo_completo)

###uso stepwise, buscando la optimizacion global del modelo, por esto,
##me va a incluir algunas variables que no son significativas individualmente
##pero que en conjunto mejoran el ajuste del modelo. (AIC más bajo que el de todas las variables)
library(MASS)
modelo_mejor <- stepAIC(modelo_completo, direction = "both")
summary(modelo_mejor)

##variables escogidas: Dosis, Hiperten*, baseEF*, SE*, ECGMI*, ECGNormal
##hago un modelo sin dosis para ver como varia el AIC

modelo_mejor2<-glm(Evento~ Hiperten+ baseEF +SE +ECG, data=train, family=binomial(link=logit))
summary(modelo_mejor2)

###el AIC aumenta un poco, lo que hace pensar que el modelo anterior es mejor,
##considerando que se penaliza por la adicion de parametros, pero en este caso el modelo con menos parametros 
#no se ajusta mejor globalmente

# Predicciones en la base de prueba
pred <- predict(modelo_mejor, newdata = test, type = "response")
# Curva ROC
roc_obj <- roc(test$Evento, pred)
plot(roc_obj, col="blue")
auc(roc_obj)


#Compare los modelos obtenidos - ROC e indicadores de la tabla de confusión (2 puntos).
#los nombro segun el numero de variables por una cosa de orden
modelo2<-modelo_final
modelo5<-modelo_mejor
library(pROC)
library(caret)

# Suponiendo que los modelos se llaman:
# modelo2 (con 2 variables)
# modelo5 (con 5 variables)

# ---- 1. Predicciones ----
# Probabilidades predichas sobre la muestra de prueba (test)
prob2 <- predict(modelo2, newdata = test, type = "response")
prob5 <- predict(modelo5, newdata = test, type = "response")

# ---- 2. ROC y AUC ----
roc2 <- roc(test$Evento, prob2)
roc5 <- roc(test$Evento, prob5)

par(mfrow = c(1,1)) # dos gráficos lado a lado
plot(roc2, col="blue", main="Comparación ROC")
lines(roc5, col="red")
legend("bottomright", legend=c("Modelo 2 vars", "Modelo 5 vars"),
       col=c("blue","red"), lwd=2)

auc(roc2)
auc(roc5)

# ---- 3. Tablas de confusión ----
# Definir umbral 0.5 (o elegir el óptimo con coords())
pred2 <- ifelse(prob2 > 0.5, 1, 0)
pred5 <- ifelse(prob5 > 0.5, 1, 0)

confusionMatrix(as.factor(pred2), as.factor(test$Evento), positive="1")
confusionMatrix(as.factor(pred5), as.factor(test$Evento), positive="1")


##optimo

# Umbral óptimo (Youden index = max(sens+esp-1))
opt2 <- as.numeric(coords(roc2, "best", ret = "threshold", best.method = "youden"))
opt5 <- as.numeric(coords(roc5, "best", ret = "threshold", best.method = "youden"))

opt2; opt5  # Umbrales recomendados

# Clasificación usando los umbrales óptimos
pred2_opt <- ifelse(prob2 > opt2, 1, 0)
pred5_opt <- ifelse(prob5 > opt5, 1, 0)

# Matrices con umbral optimo
confusionMatrix(as.factor(pred2_opt), as.factor(test$Evento), positive="1")
confusionMatrix(as.factor(pred5_opt), as.factor(test$Evento), positive="1")


###creo que dado que el valor del AIC es casi igual, aca lo que hay que profundizar
##es la habilidad de predecir
#verdadero-verdadero
#falso-falos
##y decidir cual riesgo es el que estamos dispuestas a asumir
#altos valores positivos falsos
#altos valores negativos falsos
#me imagino desde una perspectiva medica.