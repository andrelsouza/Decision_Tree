## Aplicação de Decision Tree
## Autor: André Luiz Araújo de Souza
## Problema de negócio: Criando um modelo de ML, baseado em Categorical Decision Tree, visando
## prever se a criança terá problema de Kyphosis na medula ou não
## Data de criação: 01/09/2020


setwd("C:/Users/aluiz/OneDrive/Área de Trabalho/Pós Graduação/Curso 01 - Big Data com R e Microsoft Azure Machine Learning/CAP12")
getwd()

install.packages("SciencesPo")
install.packages("caTools")

library(rpart)
library("caret")
library("randomForest")
library("datasets")
library(corrplot)
library(corrgram)
library(caTools)
library(ggplot2)
library(gridExtra)
library(moments)



# Vamos utilizar um dataset que é disponibilizado junto com o pacote rpart
str(kyphosis)
head(kyphosis)
View(kyphosis)

##### Verificando valores missing

any(is.na(kyphosis))

##### Análise Exploratória de dados

cont_table <- table(kyphosis$Kyphosis)
cont_table_prop <- round(prop.table(cont_table)*100,digits = 1)
cont_table_prop

## Analisando as variáveis numéricas

summary(kyphosis)
quantile(kyphosis$Age,seq(from = 0, to = 1, by= 0.1) )
quantile(kyphosis$Age,seq(from = 0, to = 1, by= 0.1) )
quantile(kyphosis$Number,seq(from = 0, to = 1, by= 0.1) )

## Correlação entre as variáveis numéricas

features_numericas <- sapply(kyphosis, is.numeric)
corr <- cor(kyphosis[,features_numericas])
corrplot(corr,method = "ellipse")

par(mfrow = c(1,1))
boxplot(kyphosis$Start)
g1 <- hist(kyphosis$Age)
g2 <- hist(kyphosis$Number)
g3 <- hist(kyphosis$Start)
skewness(kyphosis[,features_numericas])
kurtosis(kyphosis[,features_numericas])

grid.arrange(g1,g2,g3, nrow=1)

## Normalizando as variáveis numéricas

normalização <- function(x){
  
  return((x-min(x))/max(x)-min(x))
}

kyphosis_normalizado <- as.data.frame(lapply(kyphosis[2:4], normalização))
View(kyphosis_normalizado)

## Dividindo o dataset em treino e teste - Modelo ideal para KNN

amostra <- sample.split(kyphosis_normalizado$Age,SplitRatio = 0.7)
subset_treino<- subset(kyphosis_normalizado, amostra==TRUE)
subset_teste <- subset(kyphosis_normalizado,amostra == FALSE)

## Dividindo o dataset em treino e teste - Modelo ideal para Random Forest e SVM

kyphosis[,"Index"] <- ifelse(runif(nrow(kyphosis))<=0.7,1,0)
View(kyphosis)

subset_treino <- subset(kyphosis,kyphosis$Index==1)
subset_teste <- subset(kyphosis,kyphosis$Index==0)
subset_treino$Index <- NULL
subset_teste$Index <- NULL

View(subset_treino)
View(subset_teste)

# Criando um modelo de árvore de decisão

modelo_rf_1.0 <- rpart(Kyphosis ~ ., data = subset_treino, control = rpart.control(cp =.0005))
prediction_tree <- predict(modelo_rf_1.0, subset_teste, type = "class")

modelo_rf_1.0
## Examinando o modelo com printcp() e Confusion Matrix

# Percentual de acerto
mean(prediction_tree==subset_teste$Kyphosis)

#Confusion Matrix

table(prediction_tree, subset_teste$Kyphosis)

help(printcp)
printcp(modelo_rf_1.0)

# Visualizando a ávore (execute uma função para o plot e outra para o texto no plot)
# Utilize o zoom para visualizar melhor o gráfico


# Este outro pacote faz a visualizaco ficar mais legivel
install.packages('rpart.plot')
library(rpart.plot)
prp(modelo_rf_1.0)
