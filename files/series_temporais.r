# Definir o diretório
setwd('C:/Users/Nina/Desktop/Data-Science-R/files')
getwd()

# libs necessárias


library(tseries) #Manipular ST (Trapletti and Hornik, 2017)
library(tidyverse)
library(dplyr)
library(readxl)
library("TTR")
library(forecast)
library(lmtest)
library(FitAR)
library(randtests)
library(seasonalview)

#Leitura da base de dados

df <-
  read.csv(
    'base.csv',
    sep = "," ,
    dec = ',',
    header = T,
    stringsAsFactors = FALSE
  )

# Análise explorátoria
summary(df$volume)

# Realizar a transformaÃ§Ã£o dos dados

df$data <- as.Date(df$data, format = "%d/%m/%Y")
df$volume <- as.numeric(df$volume)

# Converter em uma série, importante especificar quando começa e a frequência
tsData = ts(df, start = c(2018,1), frequency = 12)
plot(tsData)

tsData

# # Teste de Cox-Stuart - Verifica-se hÃ¡ tendencia
cox.stuart.test(tsData)
runs.test(tsData)

timeseriescomponents <- decompose(tsData)
plot(timeseriescomponents)


#Gráfico de Autocorrelação e Autocorrelação Parcial da série
par(mfrow=c(1,2))
print(acf(df))
print(pacf(df))
Modelo

fitARIMA <- arima(tsData, order=c(1,0,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")
library(lmtest)
coeftest(fitARIMA) 

confint(fitARIMA)

# Plotar os residuos
tsdiag(fitARIMA)
plot(fitARIMA$residuals)


#Teste de hipótese dos parâmetros
library(lmtest)
coeftest(modelo)
par(mfrow=c(1,2))


coeftest(modelo)
par(mfrow=c(1,2))
#Gráfico de Autocorrelação e Autocorrelação Parcial dos resíduos
acf(residuals(modelo))
pacf(residuals(modelo))

#Projeção N passos para frente
library(forecast)
(projecao <- forecast(modelo, h=5))
