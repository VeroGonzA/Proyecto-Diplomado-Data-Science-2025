#### PARTE III - SERIES DE TIEMPO ####
library(readxl)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(tidyr)
library(FinTS)

# 1. Importar y preparar datos

des <- read_excel("tasa_des.xlsx")
des <- des %>% mutate(FECHA = as.Date(paste(AÑO, MES, "01", sep ="-")))
train <- des %>% filter(FILTRO == "train")

# Serie ts mensual: inicio 2010-03
y <- ts(train$TASA_DESOCUPACION, start = c(2010, 3), frequency = 12)

autoplot(y) + ggtitle("Tasa de Desocupación (2010–2024)") +
  xlab("Año") + ylab("Tasa en %")


# 2. ACF/PACF – Serie original

ggAcf(y, lag.max = 48) + ggtitle("ACF - Serie original (mensual)")
ggPacf(y, lag.max = 48) + ggtitle("PACF - Serie original (mensual)")


# 3. Test de estacionariedad

adf.test(y)


# 4. Box-Cox

lambda <- BoxCox.lambda(y, method = "guerrero", lower = -1, upper = 2)
use_bc <- abs(lambda - 1) > 0.1
y_bc <- if (use_bc) BoxCox(y, lambda) else y
cat(sprintf("Lambda sugerido = %.3f | ¿Usar Box-Cox?: %s\n",
            lambda, ifelse(use_bc,"Sí","No")))

# ACF/PACF tras BC
if (use_bc) {
  ggAcf(y_bc, lag.max = 48) + ggtitle(sprintf("ACF - Box-Cox (λ=%.2f)", lambda))
  ggPacf(y_bc, lag.max = 48) + ggtitle(sprintf("PACF - Box-Cox (λ=%.2f)", lambda))
}


# 5. Determinar d y D

d <- ndiffs(y_bc, alpha = 0.05, test = "adf")
D <- nsdiffs(y_bc, m = frequency(y_bc), test = "ocsb")
cat(sprintf("ndiffs (d) = %d | nsdiffs (D) = %d\n", d, D))

# Serie transformada + diferenciada para validar y mirar ACF/PACF
y_diff <- y_bc
if (D > 0) y_diff <- diff(y_diff, lag = frequency(y_diff), differences = D)
if (d > 0) y_diff <- diff(y_diff, differences = d)

adf_res <- adf.test(na.omit(y_diff)); print(adf_res)

ggAcf(na.omit(y_diff), lag.max = 48) + ggtitle("ACF - Serie transformada y diferenciada")
ggPacf(na.omit(y_diff), lag.max = 48) + ggtitle("PACF - Serie transformada y diferenciada")

# Comparar con auto.arima
fit_final <- auto.arima(
  y,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  lambda  = if (use_bc) lambda else NULL,
  biasadj = TRUE,
  d = d,
  D = D
)
summary(fit_final)


# 6. Diagnóstico Supuestos

#Residuos
checkresiduals(fit_final)
Box.test(residuals(fit_final), lag=24, type="Ljung-Box", fitdf=length(fit_final$coef))

# Test de normalidad Shapiro–Wilk
shapiro_res <- shapiro.test(residuals(fit_final))
print(shapiro_res)

#Varianza
install.packages("FinTS")
library(FinTS)

# Test ARCH con 12 rezagos
arch_res <- ArchTest(residuals(fit_final), lags = 12)
print(arch_res)


# 7. Pronóstico (ene–jun 2025)

h <- 6
fcast <- forecast(fit_final, h = h, biasadj = TRUE)
fechas_2025 <- seq(as.Date("2025-01-01"), by = "month", length.out = h)

pred <- data.frame(
  FECHA = fechas_2025,
  Predicho = as.numeric(fcast$mean),
  Lo95 = as.numeric(fcast$lower[,2]),
  Hi95 = as.numeric(fcast$upper[,2])
)

# Reales 2025
reales <- des %>%
  dplyr::filter(FECHA >= as.Date("2025-01-01") & FECHA <= as.Date("2025-06-30")) %>%
  mutate(FECHA = as.Date(format(FECHA, "%Y-%m-01"))) %>%
  group_by(FECHA) %>%
  summarise(Real = mean(TASA_DESOCUPACION, na.rm = TRUE), .groups = "drop")

comparacion <- left_join(reales, pred, by = "FECHA") %>%
  mutate(Delta = Real - Predicho,
         AbsError = abs(Delta),
         PercError = AbsError/Real*100)

print(comparacion)

MAE_2025  <- mean(comparacion$AbsError, na.rm=TRUE)
MAPE_2025 <- mean(comparacion$PercError, na.rm=TRUE)
Precision_2025 <- 100 - MAPE_2025
cat(sprintf("MAE 2025 = %.3f | MAPE 2025 = %.2f%% | Precisión = %.2f%%\n",
            MAE_2025, MAPE_2025, Precision_2025))


# 8. Gráficos finales

# (a) Serie completa + predicción
ggplot() +
  geom_line(data = des, aes(x = FECHA, y = TASA_DESOCUPACION, color = "Real")) +
  geom_line(data = pred, aes(x = FECHA, y = Predicho, color = "Predicho")) +
  geom_ribbon(data = pred, aes(x = FECHA, ymin = Lo95, ymax = Hi95), alpha = 0.15, inherit.aes = FALSE) +
  scale_color_manual(values = c("Real" = "blue", "Predicho" = "red")) +
  labs(title = "Serie histórica y Pronóstico SARIMA (2010–2025)",
       x = "Año", y = "Tasa de Desocupación (%)", color = "") +
  theme_minimal()

# (b) Zoom 2025: reales vs predichos (ene–jun)
ggplot(comparacion, aes(x = FECHA)) +
  geom_ribbon(aes(x = FECHA, ymin = Lo95, ymax = Hi95), alpha = 0.15, inherit.aes = FALSE) +
  geom_line(aes(y = Real, color = "Real"), size = 0.9) +
  geom_point(aes(y = Real, color = "Real"), size = 2) +
  geom_line(aes(y = Predicho, color = "Predicho"), linetype = "dashed", size = 0.9) +
  scale_color_manual(values = c("Real" = "blue", "Predicho" = "red")) +
  labs(title = "Reales vs Predichos (ene–jun 2025)",
       x = "Mes", y = "Tasa de Desocupación (%)", color = "") +
  theme_minimal()

