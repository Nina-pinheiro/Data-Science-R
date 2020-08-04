#Teste Shapiro é importante para verificar se os dados segue a distribuição normal, ou seja, Gaussiana.
# Se os dados segue essa distribuição, portanto os dados serão simétricos e a média = mediana = moda

# Pasta do Trabalho

setwd("C:/Users/Nina/Desktop/Machine Learning- R")

# Instalar e ler a biblioteca
#install.packages("dplyr")
#install.packages("readxl")
library(dplyr)
library(readxl)


# Leitura do dataset

dados<- read_excel("dataset/consulta.xlsx", sheet = 1)

# Visualização dos dados em uma janela separada

View(dados)

# Função que resume as variáveis se são inteiras, float e etc..

glimpse(dados)

# Uma das Variáveis é fator , no entanto no teste de shapiro, as variáveis precisa ser númericas. 
# Para confirmar utilizo a função "Sapply"

i<- sapply(dados, is.factor)

# Nesta expressão transforma as listas em dados númericos com o uso da função "lapply"

dados[i]<-lapply(dados[i], as.numeric)

# Realizo o Teste Shapiro

# É importante entender que no Teste Shapiro
# A Hipótese Nula (H0) = Se o p-value > nível de significância , então a distribuição é normal
# A Hipótese Alternativa (H1) = Se o p value < nível de significância , então a distribuição não é normal

# Nível de significância = 0.05

shapiro.test(dados$Idade)
shapiro.test(dados$Attend)

# Gráfico de Histograma 
# É importante a visualização deste gráfico, pois podemos analisar grandes assimetrias, descontinuidade de dados
# e também picos multimodais. Assim identificando que não é um padrão de distribuição normal.
 

hist(dados$Idade,  main = "Histograma Idade", xlab = "Idade", ylab = "Frequência absoluta",
                   col  = c("blue"), lables = TRUE)
hist(dados$Attend, main = "Histograma Attend", xlab = "Attend", ylab = "Frequência absoluta",
                   col  = c("violet"), labels = TRUE)