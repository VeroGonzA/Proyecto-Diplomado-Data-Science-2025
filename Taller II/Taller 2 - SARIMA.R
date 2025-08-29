#### PARTE III - SERIES DE TIEMPO ####
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(pROC)
library(forecast)
library(tseries)
library(lubridate)

# Carga de base de datos
des <- read_excel("Tasa_Des.xlsx")

# Crear columna fecha
des <- des %>%
  mutate(Fecha = ymd(paste(AÑO, MES, "01", sep = "-"))) %>%
  arrange(Fecha)

# Separar datos de entrenamiento y prueba
train <- des %>% filter(FILTRO == "train")
test <- des %>% filter(FILTRO == "test")

# Creamos la serie de tiempo con train
ts_train <- ts(train$TASA_DESOCUPACION, start =c(2010,3),
               frequency = 12)

#Graficar serie temporal
autoplot(ts_train) + ggtitle("Tasa de Desocupacion (2010/03 - 2024/12")
+ ylab("Tasa (%)")

# 1. Verificar la estacionariedad
adf.test(ts_train) #Prueba de Dickey-Fuller

## El p<.05 por ende la serie es estacionaria.
## No es necesario aplicar diferencial adicional (d=0)

# Descomposicion -> Para entender la estructura de la serie
decomp <- decompose(ts_train)
autoplot(decomp)

# STL
stl_decomp <- stl(ts_train, s.window = "periodic")
autoplot(stl_decomp)

#Interpretacion grafico
# Tendencia: Hubo un aumento gradual desde 2010, hasta el 2020 que hay un pico (debido a la pandemia)
## luego una disminucion hacia 2024 > La ts tuvo una fase de crecimiento sostenido
# Estacionalidad: muestra patrones repetitivos cada año, con fluctuaciones regulares.
## Conclusion: Hay una estacionalidad clara, probablemente relacionada con ciclos laborales anuales
## ejemplo: contraciones o despidos en ciertos meses. > Esto justifica el uso de un componente estacional en el modelo ARIMA (P,D,Q)[12]
# Ruido (Aleatoriedad): El gráfico de residuos muestra fluctuaciones irregulares sin patron claro
## El modelo capta bien la tendencia y estacionalidad, y lo que queda es ruido blanco
## Esto es deseable, ya que indica que el modelo esta explicando adecuadamente la variabilidad estructurada.

# Analisis de ACF y PACF
acf(ts_train, lag.max = 36)

#Grafico
#Las barras de autocorrelacion en los primeros lags exce las lineas punteadas,
#lo que indica autocorrelacion significativa.
#La disminucion gradual de las barras sugiere que hay una estructura
#dependiente en el tiempo, es decir, los valores pasados influyen en el los futuros.
#No hay un corte abrupto en las barras, lo que incia que el componente MA (Media Movil)
#Podria ser no dominante > La serie es estacionaria por lo tanto no necesita diferenciacion d=0
#Autocorrelacion significativa en lag 1 > MA q = 1
# Q= 1 > (p,0,1)(P,0,1)[12]

pacf(ts_train, lag.max = 36)

#Componente AR (p)
#La significancia en el lag 1 sugiere que se debe incluir un componente autoregresivo de orden 1 p:1

#Componente estacional AR (P)
#El grafico PACF no muestra lags esuacionales, si en el ACF
# se podría considerar P=1.

#Modelo arima sugerido ARIMA(1,0,1)(1,0,1)[12]

# Ejemplo de modelo manual
manual_arima <- Arima(ts_train, order = c(1,0,1), seasonal = c(1,0,1))
summary(manual_arima)

#Resumen del modelo actual: ARIMA(1,1,1)(1,1,1)[12]
#El modelo ajusta bien la serie, con bajo error y alta precisión.
#Los coeficientes estacionales son muy fuertes, lo que indica que la estacionalidad anual es clave.

#Diagnostico de residuos

checkresiduals(manual_arima)
#Al tener un p<.05 implica que hay autorrelacion significativa de los residuos
#Esto implica que le modelo no esta capturando la estrucutra de la serie

box.test(residuals(manual_arima, lag=12, type = "Ljung-Box"))

# Auto ARIMA
auto_model <- auto.arima(ts_train)
summary(auto_model)

checkresiduals(auto_model)
#Al tene p>.05 implica que NO hay autocorrelacion significativa de los residuos
#Esto implica que el modelo esta capturando la estructura de la serie.

# Comparar AIC
AIC(manual_arima)
AIC(auto_model)

# Pronóstico de 6 meses
forecast_model <- forecast(auto_model, h = 6)

# Comparar con datos reales
autoplot(forecast_model) +
  autolayer(ts(test$TASA_DESOCUPACION, start = c(2025, 1), frequency = 12), series = "Real") +
  ggtitle("Pronóstico vs Realidad: Modelo SARIMA") +
  ylab("Tasa de Desocupación")

# Crear tabla comparativa
real_values <- test$TASA_DESOCUPACION
predicted_values <- as.numeric(forecast_model$mean)
delta <- abs(real_values - predicted_values)

# Crear data frame
comparison <- data.frame(
  Mes = format(test$Fecha, "%Y-%m"),
  Valor_Real = round(real_values, 3),
  Valor_Predicho = round(predicted_values, 3),
  Delta = round(delta, 3)
)

# Calcular MAPE y precisión
mape <- mean(abs(delta / real_values)) * 100
precision <- 100 - mape

# Mostrar resultados
print(comparison)
cat("Precisión del modelo auto ARIMA:", round(precision, 2), "%\n")
