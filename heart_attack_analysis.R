# Instalar o pacote randomForest se ainda não estiver instalado
# install.packages("randomForest")

# Carregar o pacote
library(randomForest)

# Carregar os dados
dados <- read.csv("heart.csv")

dados$sex <- as.factor(dados$sex)
dados$cp <- as.factor(dados$cp)
dados$fbs <- as.factor(dados$fbs)
dados$restecg <- as.factor(dados$restecg)
dados$slp <- as.factor(dados$slp)
dados$caa <- as.factor(dados$caa)
dados$thall <- as.factor(dados$thall)
dados$output <- as.factor(dados$output)
dados$exng <- as.factor(dados$exng)
# Dividir os dados em conjuntos de treinamento e teste
set.seed(42) # Para reprodutibilidade
indices <- sample(seq_len(nrow(dados)), size = 0.8 * nrow(dados))
train_data <- dados[indices, ]
test_data <- dados[-indices, ]

# Treinar o modelo Random Forest
# Convertendo 'output' para fator
modelo_rf <- randomForest(as.factor(output) ~ ., data = train_data)

# Avaliar o modelo no conjunto de teste
predicoes <- predict(modelo_rf, test_data)

# Comparar as previsões com os valores reais
matriz_confusao <- table(
    Predicted = predicoes,
    Actual = as.factor(test_data$output)
)

# Exibir a matriz de confusão
print(matriz_confusao)

# Calculando a acurácia
acuracia <- sum(diag(matriz_confusao)) / sum(matriz_confusao)
print(paste("Acurácia:", acuracia))

