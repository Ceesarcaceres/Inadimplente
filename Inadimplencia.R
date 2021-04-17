library(Amelia)
library(ggplot2)
library(caret)
library(reshape)
library(randomForest)
library(dplyr)
library(e1071)

dados_clientes <- read.csv("dataset.csv")
View(dados_clientes)
summary(dados_clientes)
str(dados_clientes)

# Removendo a primeira coluna ID
dados_clientes$ID <- NULL
dim(dados_clientes)

# Renomeando a coluna de classe
colnames(dados_clientes)
colnames(dados_clientes)[24] <- "inadimplente"

# Verificando valores ausentes e removendo do dataset
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes)

# Convertendo os atributos genero, escolaridade, estado civil e 
# idade para fatores  
dados_clientes$SEX <- cut(dados_clientes$SEX,
                          c(0,1,2),
                          labels = c("Masculino",
                                    "Feminino"))

dados_clientes$EDUCATION <- cut(dados_clientes$EDUCATION,
                          c(0,1,2,3,4,6),
                          labels = c("graduate school",
                                     "university",
                                     "high school",
                                     "others", 
                                     "desconhecido"))
summary(dados_clientes$EDUCATION)

dados_clientes$MARRIAGE<- cut(dados_clientes$MARRIAGE,
                                c(-1,0,1,2,3),
                                labels = c("desconhecido",
                                           "married",
                                           "single",
                                           "others"))
summary(dados_clientes$MARRIAGE)

# Convertendo a variavel para o tipo fator com faixa etaria
dados_clientes$AGE<- cut(dados_clientes$AGE,
                              c(0,30,50,100),
                              labels = c("Jovem",
                                         "Adulto",
                                         "Idoso"))
summary(dados_clientes$AGE)

# Convertendo a variavel que indica pagamentos para o tipo fator
dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

# Alterando a variavel dependente para o tipo fator
str(dados_clientes$inadimplente)
dados_clientes$inadimplente <- as.factor(dados_clientes$inadimplente)

# Total de inadimplentes X n?o-inadimplentes
table(dados_clientes$inadimplente)

# Veriicar a porcentagem entre as classes
prop.table(table(dados_clientes$inadimplente))

# Plot da distribui??o usando ggplot2
qplot(inadimplente, data = dados_clientes, geom = "bar") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Set seed
set.seed(12345)

# Seleciona as linhas de acordo com a variavel inadimplente
indice <- createDataPartition(dados_clientes$inadimplente, p = 0.75, list = FALSE)
dim(indice)

# Defini??o dos dados de treino
dados_treino <- dados_clientes[indice,]
table(dados_treino$inadimplente)

prop.table(table(dados_treino$inadimplente))

# Compara??o da porcentagem entre as classes de treino X Dados originais
compara_dados <- cbind(prop.table(table(dados_treino$inadimplente)),
                       prop.table(table(dados_clientes$inadimplente)))
colnames(compara_dados) <- c("Treinamento", "Original")

# Converte colunas em linhas
melt_compara_dados <- melt(compara_dados)
melt_compara_dados

# Plot para ver a distribui??o do treino X original
ggplot(melt_compara_dados, aes(x = X1, y = value)) +
  geom_bar( aes(fill = X2), stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Transformando o dataset de teste
dados_teste <- dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treino)

# Modelo RandomForest
model_Forest <- randomForest(inadimplente ~ ., data = dados_treino)
model_Forest

# Avalia??o do modelo em gr?fico
plot(model_Forest)

# Previsoes com dados de teste
previsoes <- predict(model_Forest, dados_teste)

# Confusion Matrix
C_Matrix <- caret::confusionMatrix(previsoes, dados_teste$inadimplente, positive ="1")
C_Matrix

# Calculando Precisao, Recall e F1-Score
y <- dados_teste$inadimplente
y_pred <- previsoes

precisao <- posPredValue(y_pred, y)
precisao

recall <- sensitivity(y_pred, y)
recall

F1 <- (2 * precisao * recall) / (precisao + recall)
F1

# model_Forest <- randomForest(inadimplente ~ ., data = dados_treino_bal)

varImpPlot(model_Forest)

imp_var <- importance(model_Forest)
VarImportance <- data.frame(Variables = row.names(imp_var),
                            Importance = round(imp_var[ ,'MeanDecreaseGini'],2))
rankImportance <- VarImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

ggplot(rankImportance,
       aes(x = reorder(Variables, Importance),
           y = Importance,
           fil = Importance)) +
  geom_bar(stat='identity') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust = 0,
            vjust = 0.55,
            size = 4,
            colour = 'red') +
  labs(x = 'Variables') +
    coord_flip()

saveRDS(model_Forest, file = "E:/Inadimplencia/model_Forest.rds")