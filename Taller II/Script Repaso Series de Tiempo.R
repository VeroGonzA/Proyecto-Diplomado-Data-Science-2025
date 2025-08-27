###########################################
## DDS2025 - Modelos de Series de Tiempo ##
###########################################

###########################
## Librerías y Funciones ##
###########################

## https://cran.r-project.org/web/views/TimeSeries.html

## forecast::BoxCox.lambda(...) : Transformar datos heterocedasticos en homocedasticos
## forecast::BoxCox(...) : Aplica la transfomación
## forecast::ndiffs(...) : Numero de diferenciciones para convertor una serie en estacionaria 
## forecast::nsdiffs(...) : Numero de diferenciaciones estacionales
## forecast::Arima(...): Ajusta modelos ARMA --> SARIMAX
## forecast::auto.arima(...): Modelador experto
## forecast::forecast(...) : Predicciones + Banda de Predicción
## MASS::boxcox(...)
## LSTS::ts.diag(...)
## LSTS::Box.Ljung.Test(...)
## LSTS::periodogram(...)
## LSTS:: spectral.density(...)
## stats::ts(...)
## stats::window(...)
## stats::tsdiag(...)
## stats::arima(...)
## stats::predict(...)
## stats::Box.test(...)
## stats::ks.test(...)
## stats::shapiro.test(...)
## stats::acf(...)
## stats::pacf(...)
## stats::cor(...)
## stats::ARMAacf(...)
## stats::ARMAtoMA(...)
## stats::model.matrix(...)
## tseries::jarque.bera.test
## tseries::adf.test
## corrplot::corrplot(...)
## lmtest::bptest(...)
## lubridate::year(...)
## lubridate::month(...)
## lubridate::day(...)
## month.name(...)
## month.abb(...)

##############################
## Funciones personalizadas ##
##############################

source("summary.arima.R")
source("TS.diag.R")
source("TS.summary.R")

#################
## Modelo ARMA ##
#################

## Se aplica a series estacionarias: 
## (i ) Media constante
## (ii) Comportamiento Homocedastico
## (iii)ACF constante en el tiempo

## Ejemplo: Anillos de Crecimiento

X <- LSTS::malleco
par(bty = "n", las = 1)
plot(X, col = "gray", xlim = c(1200,2000), ylab = "", xlab = "", ylim = c(0,2))
abline(h = mean(X), lty = 2)
mod <- lm(X ~ time(X))
## Media constante
summary(mod)$coef
abline(mod, col = "red")
## Varianza constante
lmtest::bptest(mod)$p.value

## ACF vs PACF
par(bty = "n", las = 1, mfrow = c(1,2))
acf(X, ylim = c(-1,+1), xlim = c(0,10), lag.max = 10, main = "")
pacf(X, ylim = c(-1,+1), xlim = c(0,10), lag.max = 10, main = "")
## ACF --> MA(5)
##PACF --> AR(1)

## auto.arima()
mod <- forecast::auto.arima(y = X, d = 0, D = 0, max.p = 1, max.q = 5)
summary(mod)
summary.arima(fit = mod)
LSTS::Box.Ljung.Test(mod$res, lag = 20)

## MAPE del promedio a "mano"
mean(abs(mean(X)/X-1))*100

## Propuesta de mejora: ma(6) y/o ma(15)
mod <- forecast::Arima(y = X, order = c(1,0,15), fixed = c(NA,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,NA,NA))
summary(mod)
summary.arima(fit = mod)
LSTS::Box.Ljung.Test(mod$res, lag = 20)

mod <- forecast::Arima(y = X, order = c(1,0,15), fixed = c(NA,0,0,NA,0,NA,0,0,0,0,0,0,0,0,0,NA,NA))
summary(mod)
summary.arima(fit = mod)
LSTS::Box.Ljung.Test(mod$res, lag = 20)

mod <- forecast::Arima(y = X, order = c(1,0,15), fixed = c(NA,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,NA,NA))
plot(mod)

par(mfrow = c(1,1), bty = "n", las = 1)
ACF <- ARMAacf(ar = mod$coef[1], ma = mod$coef[-c(1,17)], lag.max = 20)
acf(X, ylim = c(-1,+1), xlim = c(0,20), lag.max = 20, main = "")
lines(ACF ~ c(0:20), pch = 20, col = "red", lwd = 2, type = "p")

## Predicción
pre <- forecast::forecast(mod, h = 30, level = 0.95)
par(bty = "n", las = 1, font.main = 1, mfrow = c(1,2))
plot(pre, col = "black", xlim = c(1200,2050), ylab = "", xlab = "", ylim = c(0,2))
abline(h = mean(X), lty = 2)
plot(pre, col = "black", xlim = c(1900,2010), ylab = "", xlab = "", ylim = c(0,2))
abline(h = mean(X), lty = 2)
pre

pre <- forecast::forecast(mod, h = 30, fan = T)
par(bty = "n", las = 1, font.main = 1, mfrow = c(1,2))
plot(pre, col = "black", xlim = c(1200,2050), ylab = "", xlab = "", ylim = c(0,2))
abline(h = mean(X), lty = 2)
plot(pre, col = "black", xlim = c(1900,2010), ylab = "", xlab = "", ylim = c(0,2))
abline(h = mean(X), lty = 2)
lines(mod$fitted, col = "red")

## Supuestos sobre los residuos
## Ruido Blanco [OK]
## Homocedasticidad
lmtest::bptest(lm(mod$res~time(mod$res)))$p.value
tsdiag(mod)
LSTS::ts.diag(mod$res, 20)

## El test de homocedasticidad no pasa debido a la variabilidad incial al inicio de la etapa de crecimieto
lmtest::bptest(lm(mod$res[-c(1:30)]~time(mod$res)[-c(1:30)]))$p.value

## Test Normalidad
ks.test(scale(mod$res), "pnorm")$p.value
shapiro.test(mod$res)$p.value
tseries::jarque.bera.test(mod$res)$p.value

##################
## Modelo ARIMA ##
##################

## Se aplica a series NO estacionarias con tendencia estocastica
## Despues de aplicar el operador (1-B)^d = diff(..., differences = d, lag = 1)
## se espera lograr estacionaridad y ajustar modelo ARMA

## Ejemplo: IPC

##################
## Modelo ARMAX ##
##################

## Se aplica a series NO estacionarias con tendencia deterministica
## Despues de aplicar regresión (lineal o no lineal)
## se espera lograr estacionaridad y ajustar modelo ARMA

## Ejemplo: Demanda electica

##################
## Modelo SARMA ##
##################

## Se aplica a series estacionarias: 
## (i ) Media constante
## (ii) Comportamiento Homocedastico
## (iii)ACF constante en el tiempo
## A diferencia del ARMA, este proceso permite la presencia de ARMA estacional.

## Ejemplo: Tasa de crecimiento IMACEC
 
###################
## Modelo SARIMA ##
###################

## Se aplica a series NO estacionarias con tendencia y patrón estacional estocástico
## Despues de aplicar el operador (1-B)^d = diff(..., differences = d, lag = 1) y
## (1-B^s)^S = diff(..., differences = D, lag = s)
## se espera lograr estacionaridad y ajustar modelo SARMA

####################
## Modelo SARIMAX ##
####################

## Se aplica a series NO estacionarias con tendencia y patrón estacional 
## estocástico y/o deterministico. 
## Despues de aplicar el operador regresión, 
## (1-B)^d = diff(..., differences = d, lag = 1) y 
## (1-B^s)^S = diff(..., differences = D, lag = s) 
## se espera lograr estacionaridad y ajustar modelo SARMA

########################
## Heterocedasticidad ##
########################

## En el caso que se observe comportamiento heterocedastico
## se puede aplicar previamente una transformación tipo potencia Box-Cox
## f(Y[t]) = (Y[t]^lambda-1)/lambda para lambda != 0 y f(Y[t]) = log(Y[t]) para lambda = 0.

#######################
## Demanda Electrica ##
#######################

Data <- rio::import("https://www.coordinador.cl/wp-content/uploads/2025/04/CEN-hist_ventas_de_energia.xlsx", sheet = 2)[-1,]
head(Data)
X <- ts(Data$"Total (GWh)", start = c(2000,1), frequency = 12)
par(bty = "n", las = 1)
plot(X, ylim = c(2000,7000), ylab = "GWh", xlab = "")
abline(h  = axTicks(2), lty = 2, col = "gray")
abline(v  = axTicks(1), lty = 2, col = "gray")
lines(X, lwd = 2)
lines(window(X, start = c(2023,1)), col = "red", lwd = 2)

Y <- window(X, end = c(2022,12))

## Transformamos
lambda <- round(forecast::BoxCox.lambda(Y),2)
f.Y <- forecast::BoxCox(Y, lambda)
par(mfrow = c(1,2))
plot(Y)
plot(f.Y)

####################################
## Modelo SARIMA(p,d,q)(P,D,Q)[s] ##
####################################

## ¿d, D?
d <- forecast::ndiffs(f.Y) ## d = 1
par(mfrow = c(1,1))
plot(f.Y)
par(mfrow = c(1,2))
plot(diff(f.Y, differences = d), ylim = c(-0.2,+0.2))
plot(diff(diff(f.Y, differences = d), differences = d), ylim = c(-0.2,+0.2)) ## Sobrediferenciar --> aumento en la variabilidad

par(mfrow = c(1,2))
acf(f.Y, lag.max = 60)
acf(diff(f.Y, differences = d), lag.max = 60)

D <- forecast::nsdiffs(diff(f.Y, differences = d))
par(mfrow = c(1,2))
plot(diff(diff(f.Y), lag = 12))
acf(diff(diff(f.Y), lag = 12), lag.max = 60)

## Propuesta ARMA y/o ARMA estacional
acf(diff(diff(f.Y), lag = 12), ylim = c(-1,+1), lag.max = 60)
## q = 0, Q = 1
pacf(diff(diff(f.Y), lag = 12), ylim = c(-1,+1), lag.max = 60)
## p = 0, P = 4

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

#############
## SARIMAX ##
#############

## Modelo SARIMAX con PIB
PIB <- rio::import("CCNN2018_P0_V2.xlsx", skip = 12)
head(PIB,20)
colnames(PIB) <- c("YEAR","PIB", "TASA")
head(PIB,20)
PIB$YEAR <- lubridate::year(PIB$YEAR)
head(PIB,20)
PIB$TIME <- PIB$YEAR+(12-1)/12
head(PIB,20)
plot(PIB ~ TIME, data = PIB)
## Suavizamiento NO parametrico
mod.pib <- smooth.spline(PIB$PIB ~ PIB$TIME, spar = 0)
lines(predict(mod.pib, time(X))$y)

xreg <- data.frame(PIB = predict(mod.pib, time(Y))$y/1000)
xreg <- as.matrix(xreg)
mod <- forecast::Arima(Y, lambda = lambda, xreg = xreg)
newxreg <- data.frame(PIB = predict(mod.pib, 2023+(1:12-1)/12)$y/1000)
newxreg <- as.matrix(newxreg)
pre <- forecast::forecast(mod, xreg = newxreg)
plot(pre)
lines(X)
lines(mod$fitted, col = "red")

mod <- forecast::Arima(Y, lambda = lambda, xreg = xreg, order = c(1,0,1), seasonal = c(1,1,1))
newxreg <- data.frame(PIB = predict(mod.pib, 2023+(1:12-1)/12)$y/1000)
newxreg <- as.matrix(newxreg)
pre <- forecast::forecast(mod, xreg = newxreg)
plot(pre)
lines(X)
lines(mod$fitted, col = "red")

mean(abs(pre$mean/X-1))*100


LSTS::Box.Ljung.Test(mod$res, lag = 24)
summary.arima(fit = mod)


## ELIMIAN ma1
mod <- forecast::Arima(Y, lambda = lambda, xreg = xreg, order = c(1,0,1), seasonal = c(0,1,1))
newxreg <- data.frame(PIB = predict(mod.pib, 2023+(1:12-1)/12)$y/1000)
newxreg <- as.matrix(newxreg)
pre <- forecast::forecast(mod, xreg = newxreg)
plot(pre)
lines(X)
lines(mod$fitted, col = "red")

mean(abs(pre$mean/X-1))*100


LSTS::Box.Ljung.Test(mod$res, lag = 24)
summary.arima(fit = mod)


