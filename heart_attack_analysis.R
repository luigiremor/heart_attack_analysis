# Instalar o pacote randomForest se ainda não estiver instalado
# install.packages("randomForest")

# Carregar o pacote
library(randomForest)

# Carregar os dados
dados <- read.csv("heart.csv")

vars_to_factor <- c(
    "sex",
    "cp",
    "fbs",
    "restecg",
    "exng",
    "slp",
    "caa",
    "thall",
    "output"
)
dados[vars_to_factor] <- lapply(dados[vars_to_factor], as.factor)


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

# Calculando a precisão
precisao <- matriz_confusao[2, 2] / sum(matriz_confusao[, 2])

print(paste("Precisão:", precisao))
