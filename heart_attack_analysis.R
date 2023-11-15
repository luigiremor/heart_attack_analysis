# Carregar pacotes necessários
library(caret)
library(randomForest)
library(e1071) # Para SVM
library(pROC) # Para AUC

# Carregar os dados e preparar para modelagem
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
set.seed(42)
indices <- sample(seq_len(nrow(dados)), size = 0.8 * nrow(dados))
train_data <- dados[indices, ]
test_data <- dados[-indices, ]

# Preparar dados para modelagem (converter a variável de saída em fator com dois níveis)
train_data$output <- as.factor(train_data$output)
test_data$output <- as.factor(test_data$output)

# Inicializar lista para armazenar resultados dos modelos
results <- list()

# Função para calcular métricas de desempenho
calculate_metrics <- function(predictions, actual) {
    confusion_matrix <- table(Predicted = predictions, Actual = actual)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    precision <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
    recall <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
    f1_score <- 2 * precision * recall / (precision + recall)
    auc_value <- roc(actual, as.numeric(predictions))$auc
    return(list(accuracy = accuracy, precision = precision, recall = recall, f1_score = f1_score, AUC = auc_value))
}

# Treinar e avaliar Random Forest
modelo_rf <- randomForest(output ~ ., data = train_data, ntree = 200, minsplit = 2, nodesize = 4, maxnodes = 100)
predicoes_rf <- predict(modelo_rf, test_data)
results$RandomForest <- calculate_metrics(predicoes_rf, test_data$output)

# Treinar e avaliar Regressão Logística
modelo_null <- glm(output ~ 1, family = binomial(), data = train_data)
modelo_log_step <- step(modelo_null, scope = list(lower = modelo_null, upper = glm(output ~ ., data = train_data, family = "binomial")), direction = "forward", trace = FALSE)
modelo_log <- glm(modelo_log_step, data = train_data, family = binomial())
predicoes_log <- predict(modelo_log, test_data, type = "response")
predicoes_log <- ifelse(predicoes_log > 0.5, "1", "0")
results$LogisticRegression <- calculate_metrics(as.factor(predicoes_log), test_data$output)

# Treinar e avaliar SVM
modelo_svm <- svm(output ~ ., data = train_data, probability = TRUE)
predicoes_svm <- predict(modelo_svm, test_data, probability = TRUE)
results$SVM <- calculate_metrics(predicoes_svm, test_data$output)

# Exibir resultados
print(results)
