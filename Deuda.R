library(lubridate)
library(tidyverse)
install.packages("car")
library(car)
library(astsa)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(lmtest)
library(broom)
library(kableExtra)
library(knitr)
library(MASS)
library(parallel)
library(car)
library(mlogit)
library(dplyr)
library(tidyr)
library(forecast)
library(fpp2)
library(stats)
library(quantmod)
library(tseries)


#Cargar Data PIP
dato <- read_excel("C:/Users/sanan/Downloads/Deuda.xlsx")
str(dato)

#Agregar Serie de Tiempo
datos.ts <- ts(dato, start=c(1947,1), frequency = 1)
print(datos.ts)
class(datos.ts)
end(datos.ts)
plot(datos.ts)
plot(datos.ts, main="Serie de Tiempo", ylab = "Deuda", xlab = "Tiempo")

#Test de Fuller
adf.test(datos.ts)

#Transformacion Log
datoslog <- log(datos.ts)

#Graficas de la transformacion logaritmica
plot(datoslog)
plot(datoslog, main="Serie de Tiempo", ylab = "Deuda", xlab = "Tiempo")

#Evaluacion y Transformacion de Direfencias
ndiffs(datos.ts)
datosdiff <- diff(datos.ts)

#Grafica de Diferencias
plot(datosdiff)
ndiffs(datosdiff)
acf(datosdiff)
adf.test(datosdiff)


datosdiff <- diff(datos.ts, differences =2)
plot(datosdiff)
acf(datosdiff)
adf.test(datosdiff)
par(mfrow=c(2,1))
pacf(datosdiff)

acf(ts(datosdiff,frequency=1))
pacf(ts(datosdiff,frequency=1))

#Modelo ARIMA
model <- arima(datos.ts, order = c(1,2,1))

summary(model)

tsdiag(model)


Box.test(residuals(model),type = "Ljung-Box")

error <- residuals(model)

plot(error)

#proyectando el modelo

proyeccion <- forecast::forecast(model, h=12)

proyeccion
plot(proyeccion)


