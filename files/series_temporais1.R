# Bibliotecas necessárias

library(tseries) 
library(tidyverse)
library(dplyr)
library(readxl)
library("TTR")
library(forecast)
library(lmtest)
library(FitAR)
library(randtests)
library(seasonalview)

#Definir o diretório
setwd('C:/Users/Nina/Desktop/Data-Science-R/files')
getwd()

#Leitura da base de dados

df <-
  read.csv(
    'preco.csv',
    sep = "," ,
    dec = ',',
    header = T,
    stringsAsFactors = FALSE
  )

# Transformar em série temporal

df_series = ts(df, start=c(2013,1), frequency = 12)
print(df_series)
class(df_series)

# Plotar o gráfico da série temporal

plot(df_series,  main="Série Temporal", ylab="Preço do Petróleo", col="#DA70D6", lty="dashed")

# Realizar o Teste de Estacionariedade - Como o p-value é maior que 0.5, portanto a série não é estacionária

adf.test(df_series)

# Realização da primeira diferenciação para transformar a série em estacionária

seriedif1 = diff(df_series)
plot(seriedif1)

# Realizar o Teste de Estacionariedade na primeira diferenciação - Como o p-value é maior que 0.5, portanto, a série não é estacionária

adf.test(seriedif1)

# Realizando a segunda transformação de diferenciação para transformar a série em estacionária

seriedif2 = diff(seriedif1)

# Realizando o teste de estacionariedade como o p-value é menor que 0.5, portanto a série é Estacionária

adf.test(seriedif1)

# Com a série estácionaria conseguimos realizar o modelo do arima, sabemos que o i está relacionado com a diferenciação, logo i =2
# analisar os gráficos acf e pacf para verificar os outros parâmetros do arima

# Gráfico da Série Temporal

plot(seriedif2, type="o", lty="dashed",main="Série Temporal",col="#1E90FF")

par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(seriedif2)
pacf(seriedif2)
acf(ts(seriedif2, frequency=1))
pacf(ts(seriedif2, frequency=1))

# Modelo Arima - colocando os parâmetros do arima

modelo1=arima(df_series,order=c(1,2,1))
summary(modelo1)
tsdiag(modelo1)
hist(seriedif2)

# Verificação do modelo
Box.test(residuals(modelo1),type="Ljung-Box")


# Forecast - Há uma tendendência de diminuir
forecasting=forecast::forecast(modelo1,h=10)
forecasting
plot(forecasting)

# Verificação da função autoarima
auto.arima(df_series)
