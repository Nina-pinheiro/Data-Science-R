setwd("C:/Users/Nina/Desktop/machinelearning_estatistica_R/dataset")
library(readxl)

# Problema: Quero prever quanto será o salário com base  no passar dos anos 


# Sep é um separador de coluna e o dec = separador de casas decimais.
dados <- read.table("salary.csv", header = TRUE,
                   sep = ",", dec = ".") #Função que lê o arquivo csv e salva em uma base de dados R 

# Verifica os nomes das variaveis
names(dados)

# Verifica a correlação linear entre duas variáveis quantitativas
cor(dados$years, dados$salary)

# O output é +0.97. Portanto é uma correlação linear forte e positiva, ou seja, com o passar dos anos aumenta o meu salário

# Realizando o modelo de Regressão Linear

# Obtenha o modelo de regressão linear simples. Com 90% de confiança, há relação linear entre as variáveis?
regressao <- lm(data=dados,
                salary ~ years)  
options(scipen=999)
summary(regressao)

# De acordo co o output temos BO = 25792.2, significa que quando você tem 0 anos , ou seja, nem nasceu, você já tem um salário assim.
# Podemos notar que a base de dados não é verossímel a realidade.

regressao$coefficients[1] + regressao$coefficients[2] * 1

# Calcula os valores preditos da varíavel resposta para cada elemento da amostra
regressao$fitted.values #: calcula os valores preditos da variável resposta(Y) para cada elemento da amostra (faz uma previsão);

# calcula o erro ou os resíduos (valor observado - valor predito) para cada ponto da amostra
regressao$residuals 

# obtém uma estimativa dos coeficientes da regressão.
regressao$coefficients 

#  Gerar o gráfico de dispersão - variáveis quantitativas

plot (salary ~ years,pch = 4, data = dados)

# Esta função ajusta a reta do modelo aos dados
abline(regressao,col="red")

# Aqui a gente consegue ver um raio quadrado bem grande, ou seja, a interpretação disso 95% da variabilidade explicada da variável y pela x.
summary(regressao)


setwd("C:/Users/Nina/Desktop/machinelearning_estatistica_R/dataset")
library(readxl)

# Problema: Quero prever quanto será o salário com base  no passar dos anos 


# Sep é um separador de coluna e o dec = separador de casas decimais.
dados <- read.table("salary.csv", header = TRUE,
                   sep = ",", dec = ".") #Função que lê o arquivo csv e salva em uma base de dados R 

# Verifica os nomes das variaveis
names(dados)

# Verifica a correlação linear entre duas variáveis quantitativas
cor(dados$years, dados$salary)

# O output é +0.97. Portanto é uma correlação linear forte e positiva, ou seja, com o passar dos anos aumenta o meu salário

# Realizando o modelo de Regressão Linear

# Obtenha o modelo de regressão linear simples. Com 90% de confiança, há relação linear entre as variáveis?
regressao <- lm(data=dados,
                salary ~ years)  
options(scipen=999)
summary(regressao)

# De acordo co o output temos BO = 25792.2, significa que quando você tem 0 anos , ou seja, nem nasceu, você já tem um salário assim.
# Podemos notar que a base de dados não é verossímel a realidade.

regressao$coefficients[1] + regressao$coefficients[2] * 1

# Calcula os valores preditos da varíavel resposta para cada elemento da amostra
regressao$fitted.values #: calcula os valores preditos da variável resposta(Y) para cada elemento da amostra (faz uma previsão);

# calcula o erro ou os resíduos (valor observado - valor predito) para cada ponto da amostra
regressao$residuals 

# obtém uma estimativa dos coeficientes da regressão.
regressao$coefficients 

#  Gerar o gráfico de dispersão - variáveis quantitativas

plot (salary ~ years,pch = 4, data = dados)

# Esta função ajusta a reta do modelo aos dados
abline(regressao,col="red")

# Aqui a gente consegue ver um raio quadrado bem grande, ou seja, a interpretação disso 95% da variabilidade explicada da variável y pela x.
summary(regressao)


