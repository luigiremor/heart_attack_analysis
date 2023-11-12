# Carregando pacotes necessários
library(randomForest)
library(caret)

# Carregando os dados
data <- read.csv("./heart.csv")

data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exng <- as.factor(data$exng)
data$slp <- as.factor(data$slp)
data$caa <- as.factor(data$caa)
data$thall <- as.factor(data$thall)

# Separando características e rótulos
features <- data[, names(data) != "output"]
labels <- data$output

# Dividindo os dados em conjuntos de treino e teste
set.seed(42) # Para reprodutibilidade
index <- createDataPartition(labels, p = .8, list = FALSE)
train_features <- features[index, ]
train_labels <- labels[index]
test_features <- features[-index, ]
test_labels <- labels[-index]

# Normalizando as características
train_features_scaled <- scale(train_features)
test_features_scaled <- scale(test_features, center = attr(train_features_scaled, "scaled:center"), 
                              scale = attr(train_features_scaled, "scaled:scale"))

# Treinando o modelo Random Forest
rf_model <- randomForest(x = train_features_scaled, y = train_labels, ntree = 100)

# Fazendo previsões e avaliando o modelo
predictions <- predict(rf_model, test_features_scaled)
conf_matrix <- confusionMatrix(predictions, test_labels)

# Exibindo a precisão e o relatório de classificação
print(conf_matrix$overall['Accuracy'])
print(conf_matrix)

