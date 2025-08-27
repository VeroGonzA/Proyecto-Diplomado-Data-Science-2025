library(readxl)
Tasa_Des <- read_excel("Taller II/Tasa_Des.xlsx")
View(Tasa_Des)


# Paquetes
library(readxl)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(tseries)
library(FinTS)
library(Metrics)

# ==============
# hacer base de entrenamiento y prueba

train<-Tasa_Des |> 
  filter(FILTRO=="train")

test<-Tasa_Des |> 
  filter(FILTRO=="test")
# ==============
# 2) Ajuste del modelo SARIMA con auto.arima (luego validamos supuestos)
# ==============
set.seed(14)
modelo <- auto.arima(train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
print(modelo)
cat("\nModelo seleccionado (ARIMA):", arimaorder(modelo), "\n")

# ==============
# 3) Verificación de supuestos
# ==============

# 3.1 Estacionariedad (ADF y KPSS) sobre train_ts (tal como se modela)
adf_res  <- tryCatch(adf.test(train_ts), error = function(e) e)
kpss_res <- tryCatch(kpss.test(train_ts, null = "Level"), error = function(e) e)

cat("\n--- Estacionariedad ---\n")
print(adf_res)
print(kpss_res)

# 3.2 Tendencia/estacionalidad: descomposición STL (visual) + que residuos del modelo no la contengan
if (freq == 12) {
  decomp <- stl(ts(train_ts, frequency = 12), s.window = "periodic")
} else {
  decomp <- stl(ts(train_ts, frequency = 4), s.window = "periodic")
}
# plot(decomp)  # Puedes activar para ver

# 3.3 Normalidad de residuos (Jarque-Bera y Shapiro + QQ plot)
resid_m <- residuals(modelo)

cat("\n--- Normalidad de residuos ---\n")
jb_res <- tryCatch(jarque.bera.test(resid_m), error = function(e) e)
print(jb_res)
if (length(resid_m) <= 5000) {  # Shapiro tiene límite práctico
  print(shapiro.test(resid_m))
} else {
  cat("Shapiro omitido por tamaño muestral.\n")
}
# qqnorm(resid_m); qqline(resid_m, col="red")  # activar si deseas ver

# 3.4 Homocedasticidad (Engle ARCH)
cat("\n--- Homocedasticidad (Engle ARCH) ---\n")
print(ArchTest(resid_m))  # H0: no hay heterocedasticidad

# 3.5 Independencia (Ljung-Box) + ACF de residuos
cat("\n--- Independencia (Ljung-Box) ---\n")
lag_lb <- ifelse(freq == 12, 24, 8)
print(Box.test(resid_m, lag = lag_lb, type = "Ljung-Box"))

# Revisión integrada de residuos (incluye ACF/PACF y Ljung-Box gráfico)
# checkresiduals(modelo)  # activar para ver gráficos

# ==============
# 4) Pronóstico primer semestre 2025 y comparación con reales
# ==============
h <- length(test_ts)
fc <- forecast(modelo, h = h)

# Tabla de comparación
comparacion <- data.frame(
  Periodo = if (freq == 12) {
    # construir etiquetas YYYY-MM
    ym(seq.Date(from = as.Date(if (freq==12) "2025-01-01" else "2025-01-01"),
                by = if (freq==12) "month" else "quarter",
                length.out = h)) %>% as.character()
  } else {
    paste0("Q", cycle(test_ts), "-", floor(time(test_ts)))
  },
  Real = as.numeric(test_ts),
  Pronosticado = as.numeric(fc$mean),
  LI80 = as.numeric(fc$lower[,1]),
  LS80 = as.numeric(fc$upper[,1]),
  LI95 = as.numeric(fc$lower[,2]),
  LS95 = as.numeric(fc$upper[,2])
)

comparacion <- comparacion %>%
  mutate(Error = Real - Pronosticado,
         APE = abs(Error) / Real * 100)

print(comparacion)

# Métricas
rmse_val <- rmse(comparacion$Real, comparacion$Pronosticado)
mae_val  <- mae(comparacion$Real, comparacion$Pronosticado)
mape_val <- mean(comparacion$APE, na.rm = TRUE)

metricas <- data.frame(
  RMSE = rmse_val,
  MAE  = mae_val,
  MAPE = mape_val
)
print(metricas)

# ==============
# 5) Gráficos
# ==============
# Serie + pronóstico (con reales en 2025)
autoplot(fc) +
  autolayer(test_ts, series = "Real 2025") +
  labs(title = "Tasa de Desocupación - SARIMA: pronóstico vs real",
       x = "Tiempo", y = "Tasa (%)") +
  theme_minimal()

# Resumen visual de residuos
#  - ACF residuos
#  - QQ-plot
par(mfrow = c(1,2))
acf(resid_m, main = "ACF de residuos")
qqnorm(resid_m); qqline(resid_m, col = "red")
par(mfrow = c(1,1))

# (Opcional) Guardar tablas a CSV
# write.csv(comparacion, "comparacion_pronostico_2025S1.csv", row.names = FALSE)
# write.csv(metricas,    "metricas_2025S1.csv", row.names = FALSE)
