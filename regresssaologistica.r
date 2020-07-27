# Goal: Determine whether or not a client attended the consultation


# Remove Scientific Notation


options(scipen=999) 

# Work Folder 

setwd("C:/Users/Nina/Desktop/Machine Learning- R")

#Required Library

# Library required to import excel files
#install.packages("readxl") 

# Warns "R" that uses this library

library(readxl) 

# Creation of a function that reads the "xls" file and saved in a database "R"

base<- read_excel("dataset/consulta.xlsx", sheet=1)

# Show the base variables
names(base)

# Performing a descriptive analysis of the data


# For the variable "Sexo"

table(base$Sexo) # Display the absolute frequency

# For the variable "Idade"

summary(base$Idade) # Display, for quantitative variables, the main measures of position

boxplot(base$Idade) # Display the box-plot

# For the variable "Cidade"

table(base$Cidade) # Display the absolute frequencies

# For the variable "Attend"

table(base$Attend) # Display the absolute frequencies
prop.table(table(base$Attend)) # Display the relative frequencies


# Transform a quantitative variable into a qualitative one

base$Attend <- as.factor(base$Attend)

# Logistic Regression Model

# Create the model using GLM (general linear model)
# Binomial family - two possibilities - 0 or 1
#Link = logit uses the logistic function
modelo <-glm(Attend ~ 
               Sexo
                + Idade 
                +Cidade, 
                family = binomial (link='logit'), data = base)

summary(modelo) # Display the Model










