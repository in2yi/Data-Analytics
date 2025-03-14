if (!require(ggplot2)) install.packages("ggplot2")
if (!require(class)) install.packages("class")
if (!require(caret)) install.packages("caret")
library(ggplot2)
library(class)
library(caret)

wine <- read.csv("lab4/wine.data", header = FALSE)
colnames(wine) <- c("Type", "Alcohol", "Malic_Acid", "Ash", "Alcalinity", 
                    "Magnesium", "Phenols", "Flavanoids", "Nonflavanoid_Phenols", 
                    "Proanthocyanins", "Color_Intensity", "Hue", "OD280_OD315", "Proline")

# Compute the PCs and plot the dataset using the 1st and 2nd PC.
pca_full <- prcomp(wine[,-1], center = TRUE, scale. = TRUE)

pca_scores <- as.data.frame(pca_full$x)
pca_scores$Type <- as.factor(wine$Type)

ggplot(pca_scores, aes(x = PC1, y = PC2, color = Type)) +
  geom_point(size = 3) +
  labs(title = "PCA of Wine Data: PC1 vs PC2", x = "PC1", y = "PC2")


# Identify the variables that contribute the most to the 1st PC.
loadings_pc1 <- pca_full$rotation[, 1]
abs_loadings <- abs(loadings_pc1)

sorted_loadings <- sort(abs_loadings, decreasing = TRUE)
print(sorted_loadings)

top_n <- ceiling(length(sorted_loadings) * 0.5)
top_vars <- names(sorted_loadings)[1:top_n]
cat("Top variables contributing to PC1:", paste(top_vars, collapse = ", "), "\n")


# Drop the variables least contributing to the 1st PC and rerun PCA.
wine_reduced <- wine[, c("Type", top_vars)]

pca_reduced <- prcomp(wine_reduced[,-1], center = TRUE, scale. = TRUE)
summary(pca_reduced)

# Train a classifier model (e.g. kNN) to predict wine type using the original dataset.
set.seed(123)
trainIndex <- createDataPartition(wine$Type, p = 0.7, list = FALSE)
train <- wine[trainIndex, ]
test  <- wine[-trainIndex, ]

train_scaled <- scale(train[,-1])
test_scaled  <- scale(test[,-1], center = attr(train_scaled, "scaled:center"), 
                      scale = attr(train_scaled, "scaled:scale"))

pred_knn_orig <- knn(train_scaled, test_scaled, cl = train$Type, k = 3)

confusion_orig <- confusionMatrix(pred_knn_orig, as.factor(test$Type))
print(confusion_orig)

#Train a classifier model to predict wine type using the data projected into the first 3 PCs
pca_train <- prcomp(train[,-1], center = TRUE, scale. = TRUE)

train_pc3 <- pca_train$x[, 1:3]

test_pc3 <- predict(pca_train, newdata = test[,-1])[, 1:3]

pred_knn_pc3 <- knn(train_pc3, test_pc3, cl = train$Type, k = 3)

confusion_pc3 <- confusionMatrix(pred_knn_pc3, as.factor(test$Type))
print(confusion_pc3)

#Compare the 2 classification models using contingency tables and prevision/recall/f1 metrics
cat("Confusion Matrix for kNN on Original Data:\n")
print(confusion_orig$table)
cat("\nConfusion Matrix for kNN on PCA-based Data:\n")
print(confusion_pc3$table)

compute_f1 <- function(conf_mat, positive_class) {
  tp <- conf_mat[positive_class, positive_class]
  fp <- sum(conf_mat[, positive_class]) - tp
  fn <- sum(conf_mat[positive_class, ]) - tp
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  return(c(precision = precision, recall = recall, f1 = f1))
}

metrics_orig <- lapply(levels(as.factor(test$Type)), function(cls) {
  compute_f1(confusion_orig$table, cls)
})
names(metrics_orig) <- levels(as.factor(test$Type))
cat("\nMetrics for kNN on Original Data:\n")
print(metrics_orig)

metrics_pc3 <- lapply(levels(as.factor(test$Type)), function(cls) {
  compute_f1(confusion_pc3$table, cls)
})
names(metrics_pc3) <- levels(as.factor(test$Type))
cat("\nMetrics for kNN on PCA-based Data:\n")
print(metrics_pc3)







