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

head(Tasa_Des)
X <- ts(Tasa_Des$TASA_DESOCUPACION , start = c(2010,1), frequency = 12)
par(bty = "n", las = 1)
plot(X, ylab = "Tasa Desocupacion", xlab = "")
abline(h  = axTicks(2), lty = 2, col = "gray")
abline(v  = axTicks(1), lty = 2, col = "gray")
lines(X, lwd = 2)
lines(window(X, start = c(2025,1)), col = "red", lwd = 2)#la linea roja es lo que debemos poder predecir

Y <- window(X, end = c(2024,12))

###V: sin ajustar por lambda primero

####################################
## Modelo SARIMA(p,d,q)(P,D,Q)[s] ##
####################################
#d:serie con tendencia ya no tiene tendencia, "que grado de diferenciacion requiero para que desaparezca la tendencia"
#D:  que grado de diferenciacion requiero para eliminar la estacionalidad
## ¿d, D?
#d: obtener un d que elimine la tendencia
d <- forecast::ndiffs(Y) ## d = 1
par(mfrow = c(1,1))
plot(Y)
par(mfrow = c(1,2))
plot(diff(Y, differences = d),ylim = c(-2,2))
plot(diff(diff(Y, differences = d), differences = d),ylim = c(-2,2)) ## Sobrediferenciar --> aumento en la variabilidad

par(mfrow = c(1,2))
acf(Y, lag.max = 240)
acf(diff(Y, differences = d), lag.max = 60)

D <- forecast::nsdiffs(diff(Y, differences = d))
par(mfrow = c(1,2))
plot(diff(diff(Y), lag = 12))

###transformacion de lambda =-1

lambda <- round(forecast::BoxCox.lambda(Y),2)
f.Y <- forecast::BoxCox(Y, lambda)
par(mfrow = c(1,2))
plot(Y)
plot(f.Y)

d <- forecast::ndiffs(f.Y) ## d = 1
par(mfrow = c(1,1))
plot(Y)
par(mfrow = c(1,2))
plot(diff(Y, differences = d),ylim = c(-2,2))
plot(diff(diff(f.Y, differences = d), differences = d),ylim = c(-2,2)) ## Sobrediferenciar --> aumento en la variabilidad



D <- forecast::nsdiffs(diff(f.Y, differences = d))
par(mfrow = c(1,1))
plot(diff(diff(f.Y), lag = 12))

par(mfrow = c(1,2))
acf(f.Y, lag.max = 60)
acf(diff(f.Y, differences = d), lag.max = 60)


## Propuesta ARMA y/o ARMA estacional
acf(diff(diff(f.Y), lag = 12), ylim = c(-1,+1), lag.max = 60)
## q = 0, Q = 1
pacf(diff(diff(f.Y), lag = 12), ylim = c(-1,+1), lag.max = 60)
## p = 0, P = 3

mod <- forecast::auto.arima(Y, d = 1, D = 1, max.p = 0, max.q = 0, max.P = 4, max.Q = 1, allowdrift = F, lambda = lambda)
mod
par(mfrow = c(1,1))
plot(mod)
summary.arima(fit = mod)
TS.summary(Y, fit = mod, fixed = c(NA))
LSTS::Box.Ljung.Test(mod$res, lag = 24)

## Vamos a mejorar el modelo incororando secuencialmente ma1, ma4, ma13 y ma16
mod <- forecast::Arima(Y, lambda = lambda, order = c(0,1,1), seasonal = c(0,1,1))
mod
summary.arima(fit = mod)
LSTS::Box.Ljung.Test(mod$res, lag = 24)

mod <- forecast::Arima(Y, lambda = lambda, order = c(0,1,4), seasonal = c(0,1,1), fixed = c(NA,0,0,NA, NA))
mod
summary.arima(fit = mod)
LSTS::Box.Ljung.Test(mod$res, lag = 24)

mod <- forecast::Arima(Y, lambda = lambda, order = c(0,1,4), seasonal = c(0,1,1), fixed = c(NA,NA,0,NA, NA))
mod
summary.arima(fit = mod)
LSTS::Box.Ljung.Test(mod$res, lag = 24)

mod <- forecast::Arima(Y, lambda = lambda, order = c(0,1,4), seasonal = c(0,1,1), fixed = c(0,0,0,0, NA))
mod
summary.arima(fit = mod)
LSTS::Box.Ljung.Test(mod$res, lag = 24)

## Nos quedamos con el auto.arima
pre <- forecast::forecast(mod, h = 12, level = 0.95)
plot(pre)
lines(X)

plot(pre, xlim = c(2020, 2025))
lines(X)

plot(pre)
lines(X)
lines(mod$fitted, col = "red")

mean(abs(pre$mean/X-1))*100

lmtest::bptest(lm(mod$res~time(mod$res)))$p.value
ks.test(scale(mod$res), "pnorm")$p.value

## Ejemplo de modelo con super sobreajuste (se modela cada dato)
mod <- lm(Y ~ as.factor(c(time(Y))))
plot(Y)
lines(mod$fitted.values ~ c(time(Y)), col = "red")   
